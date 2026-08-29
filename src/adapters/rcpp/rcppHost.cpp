// R-backed implementations of the jaspHost seam.

#include <Rcpp.h>
#include "jaspObject.h"
#include "jaspHost.h"
#include "jaspResults.h"
#include "rcppPlot.h"
#include "rcppResults.h"

void setJaspLogFunction(Rcpp::XPtr<logFuncDef> func)
{
	jaspHost::logString = *func;

	if(jaspHost::logString)
		jaspHost::logString("Log string function received loud and clear!");
}

void rcppWireHostStore()
{
	jaspHost::storeObject = [](const std::string & envName, std::any obj)
	{
		Rcpp::RObject rObj = obj.has_value() ? std::any_cast<Rcpp::RObject>(obj) : Rcpp::RObject(R_NilValue);
		rcppSetObjectInEnv(envName, rObj);
	};

	jaspHost::fetchObject = [](const std::string & envName) -> std::any
	{
		return std::any(rcppGetObjectFromEnv(envName));
	};

	jaspHost::objectExists = [](const std::string & envName)
	{
		return rcppObjectExistsInEnv(envName);
	};

	jaspHost::clearObjects = []()
	{
		// The R storage environment is cleared from the R side (.onAttach in zzzWrappers.R)
	};

	jaspHost::destroyObjectStore = []()
	{
		rcppDestroyStorageEnv();
	};

	jaspHost::signalAnalysisAbort = []()
	{
		static Rcpp::Function signalAnalysisAbort = Rcpp::Environment::namespace_env("jaspBase")["signalAnalysisAbort"];
		signalAnalysisAbort();
	};

	jaspHost::saveStateArchive = [](jaspResults & results, const std::string & jsonPath)
	{
		rcppSaveResultsAsRds(results, jsonPath);
	};

	jaspHost::renderPlot = [](jaspPlot & plot)
	{
		rcppRenderPlot(plot);
	};

	jaspHost::plotStateSync = [](jaspPlot & plot)
	{
		rcppSetUserPlotChangesFromRStateObject(plot);
	};
}
