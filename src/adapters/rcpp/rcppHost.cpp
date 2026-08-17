// R-backed implementations of the jaspHost seam.
// Phase 1 starts with the log function; send/poll/abort/decode/render/state
// follow as their core classes move (commits 04-08).

#include <Rcpp.h>
#include "jaspObject.h"
#include "jaspHost.h"

void setJaspLogFunction(Rcpp::XPtr<logFuncDef> func)
{
	jaspHost::logString = *func;

	if(jaspHost::logString)
		jaspHost::logString("Log string function received loud and clear!");
}
