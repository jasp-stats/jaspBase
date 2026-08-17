#pragma once

// R-backed implementations of the jaspHost seam (see src/core/jaspHost.h).
// Phase 1: log function. send/poll/abort/decode/render/state follow as their
// core classes move.

#include <Rcpp.h>
#include "jaspObject.h" // logFuncDef

void		setJaspLogFunction( Rcpp::XPtr<logFuncDef> func );
