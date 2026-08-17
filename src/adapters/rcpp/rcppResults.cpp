// R-half of jaspResults; moved verbatim (behaviour and field names kept
// identical) from the old src/jaspResults.{h,cpp}. The R-free core lives in
// src/core/jaspResults.{h,cpp} and reaches everything R-specific through
// jaspHost callbacks installed here.

#include <cstdlib>
#include <cstring>

#include "rcppResults.h"
#include "rcppHost.h"
#include "rcppPlot.h"
#include "rcppToRObject.h"
#include "jaspHost.h"
#include "jaspState.h"

// The R binding layer itself: RCPP_MODULE(jaspResults) + all *_Interface
// registrations (moved from the old src/jaspResults.cpp include).
#include "jaspModuleRegistration.h"

// We need this environment to store R objects in a "named" fashion, because
// then the garbage collector doesn't throw away everything...
// Inside JASP a fresh child of the global env (reachable as
// globalenv()$RStorageEnv), otherwise jaspBase's .plotStateStorage. Moved
// from the old static jaspResults::_RStorageEnv member.
static Rcpp::Environment * _RStorageEnv = nullptr;

static void rcppCreateStorageEnv()
{
	if(_RStorageEnv != nullptr)
		delete _RStorageEnv;

	if(jaspResults::isInsideJASP())
	{
		Rcpp::Environment::global_env()["RStorageEnv"] = Rcpp::Environment::global_env().new_child(true);
		_RStorageEnv = new Rcpp::Environment(Rcpp::Environment::global_env()["RStorageEnv"]);
	}
	else
		_RStorageEnv = new Rcpp::Environment(Rcpp::as<Rcpp::Environment>(Rcpp::Environment::namespace_env("jaspBase")[".plotStateStorage"]));
}

void rcppDestroyStorageEnv()
{
	if(_RStorageEnv != nullptr)
		delete _RStorageEnv;

	_RStorageEnv = nullptr;
}

/// The storage env used to be created only in the jaspResults constructor,
/// so a jaspState/jaspPlot store access before any jaspResults existed
/// dereferenced nullptr. Lazily create it with the same inside/outside-JASP
/// selection instead.
static void rcppEnsureStorageEnv()
{
	if(_RStorageEnv == nullptr)
		rcppCreateStorageEnv();
}

Rcpp::RObject rcppGetObjectFromEnv(std::string envName)
{
	rcppEnsureStorageEnv();
	if(_RStorageEnv->exists(envName))
		return (*_RStorageEnv)[envName];
	return R_NilValue;
}

void rcppSetObjectInEnv(std::string envName, Rcpp::RObject obj)
{
	rcppEnsureStorageEnv();
	(*_RStorageEnv)[envName] = obj;
}

bool rcppObjectExistsInEnv(std::string envName)
{
	rcppEnsureStorageEnv();
	return _RStorageEnv->exists(envName);
}

void rcppSetSendFunc(Rcpp::XPtr<sendFuncDef> sendFunc)
{
	jaspResults::setSendFunc(*sendFunc);
}

void rcppSetPollMessagesFunc(Rcpp::XPtr<pollMessagesFuncDef> pollFunc)
{
	jaspResults::setPollMessagesFunc(*pollFunc);
}

void rcppStaticStartProgressbar(int expectedTicks, Rcpp::String label)
{
	jaspResults::staticStartProgressbar(expectedTicks, std::string(label));
}

Rcpp::String rcppWriteSealFilename()
{
	return jaspResults::writeSealFilename();
}

void rcppFillEnvironmentWithStateObjects(Rcpp::List state)
{
	if(state.containsElementNamed("figures"))
	{
		//Let's try to load all previous plots from the state!
		Rcpp::List figures = state["figures"];

		for(Rcpp::List plotInfo : figures)
			if(plotInfo.containsElementNamed("envName") && plotInfo.containsElementNamed("obj"))
			{
				std::string envName = Rcpp::as<std::string>(plotInfo["envName"]);
				(*_RStorageEnv)[envName] = plotInfo;
			}
	}

	if(state.containsElementNamed("other"))
	{
		//Let's try to load all previous plots from the state!
		Rcpp::List others = state["other"];
		Rcpp::List names  = others.names();

		for(std::string name : names)
			(*_RStorageEnv)[name] = others[name];
	}
}

void rcppSaveResultsAsRds(jaspResults & results, const std::string & jsonPath)
{
	if (std::getenv("JASP_RESULTS_RDS") == nullptr) return;

	// Also write results as an RDS file alongside the JSON
	std::string rdsPath = jsonPath;
	size_t dotPos = rdsPath.rfind(".json");
	if(dotPos != std::string::npos)
		rdsPath.replace(dotPos, 5, ".rds");
	else
		rdsPath += ".rds";

	// By default, strip bulky environments and plot objects from the
	// RDS tree before saving. This keeps the file small (KB) for
	// consumers like RoboReport.
	// Users can opt out to get the full toRObject() tree (e.g. for
	// debugging) by setting the env var: JASP_RDS_STRIP=FALSE (or 0/no)
	Rcpp::RObject rdsObject = rcppToRObject(&results);
	const char* stripEnvVal = std::getenv("JASP_RDS_STRIP");
	bool shouldStrip = (stripEnvVal == nullptr) ||
		(strcmp(stripEnvVal, "FALSE") != 0 && strcmp(stripEnvVal, "0") != 0 &&
		 strcmp(stripEnvVal, "NO")   != 0 && strcmp(stripEnvVal, "no")   != 0 &&
		 strcmp(stripEnvVal, "No")   != 0);
	if (shouldStrip)
	{
		Rcpp::Environment jaspBaseEnv = Rcpp::Environment::namespace_env("jaspBase");
		Rcpp::Function stripEnv = jaspBaseEnv[".jaspResults_stripEnv"];
		rdsObject = stripEnv(rdsObject);
	}
	Rcpp::Function saveRDS("saveRDS");
	saveRDS(rdsObject, rdsPath);
	jaspPrint("Saved jaspResults as RDS to: '" + rdsPath + "'");
}

jaspResults * rcppCreateJaspResults(Rcpp::String title, Rcpp::RObject oldState)
{
	rcppWireHostStore();	// idempotent: point jaspHost at the R env store + R callbacks
	rcppCreateStorageEnv();	// the old constructor's _RStorageEnv (re)creation

	jaspResults * results = new jaspResults(std::string(title));

	bool imNotReincarnatedAfterBeingMurdered = results->lastWriteWorked();

	if(imNotReincarnatedAfterBeingMurdered && !oldState.isNULL() && Rcpp::is<Rcpp::List>(oldState))
		rcppFillEnvironmentWithStateObjects(Rcpp::as<Rcpp::List>(oldState));

	results->loadResultsIfLastWriteWorked();

	return results;
}

Rcpp::List rcppGetPlotObjectsForState(jaspResults * results)
{
	Rcpp::List returnThis;
	Rcpp::Shield<Rcpp::List> protectList(returnThis);

	for(const jaspPlotStateEntry & entry : results->harvestPlotObjects())
	{
		Rcpp::List pngImg;
		pngImg["obj"]					= rcppGetPlotObjectFromEnvName(entry.envName);
		pngImg["width"]					= entry.width;
		pngImg["height"]				= entry.height;
		pngImg["revision"]				= entry.revision;
		pngImg["envName"]				= entry.envName;
		pngImg["getUnique"]				= entry.uniqueNestedName;
		returnThis[entry.filePathPng]	= pngImg;
	}

	return returnThis;
}

Rcpp::List rcppGetOtherObjectsForState(jaspResults * results)
{
	Rcpp::List returnThis;
	Rcpp::Shield<Rcpp::List> protectList(returnThis);

	for(const std::string & envName : results->harvestStateEnvNames())
	{
		std::any stored = jaspHost::fetchObject(envName);
		if(stored.has_value())
			returnThis[envName] = std::any_cast<Rcpp::RObject>(stored);
	}

	return returnThis;
}

Rcpp::List rcppGetKeepList(jaspResults * results)
{
	std::vector<std::string> keepVec = results->getKeepListVector();

	Rcpp::List keep(static_cast<R_xlen_t>(keepVec.size()));
	for(size_t i = 0; i < keepVec.size(); i++)
		keep[i] = keepVec[i];

	return keep;
}
