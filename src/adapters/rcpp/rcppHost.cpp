// R-backed implementations of the jaspHost seam.
// Phase 1 starts with the log function; send/poll/abort/decode/render/state
// follow as their core classes move (commits 04-08).

#include <Rcpp.h>
#include "jaspObject.h"
#include "jaspHost.h"
#include "jaspResults.h"

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
		jaspResults::setObjectInEnv(envName, rObj);
	};

	jaspHost::fetchObject = [](const std::string & envName) -> std::any
	{
		return std::any(jaspResults::getObjectFromEnv(envName));
	};

	jaspHost::objectExists = [](const std::string & envName)
	{
		return jaspResults::objectExistsInEnv(envName);
	};

	jaspHost::clearObjects = []()
	{
		// The R storage environment is cleared from the R side (.onAttach in zzzWrappers.R)
	};
}
