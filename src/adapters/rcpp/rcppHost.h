#pragma once

// R-backed implementations of the jaspHost seam (see src/core/jaspHost.h).
// Installed by rcppWireHostStore(), called from the jaspResults constructor.

#include <Rcpp.h>
#include "jaspObject.h" // logFuncDef

void		setJaspLogFunction( Rcpp::XPtr<logFuncDef> func );

/// Points the jaspHost object store at jaspResults::_RStorageEnv so that
/// R objects stored by jaspState/jaspPlot stay protected from R's GC, and
/// installs the R-backed plot rendering / state-sync callbacks.
/// Idempotent; called from the jaspResults constructor.
void		rcppWireHostStore();
