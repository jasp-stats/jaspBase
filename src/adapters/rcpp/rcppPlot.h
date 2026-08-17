#pragma once

// R-backed plot helpers: the original jaspPlot rendering/state logic that
// depends on R (tryToWriteImageJaspResults, stored R plot objects, toRObject).

#include <Rcpp.h>

class jaspPlot;

void			rcppRenderPlot(jaspPlot & plot);
Rcpp::RObject	rcppGetPlotObject(jaspPlot * plot);
Rcpp::RObject	rcppGetPlotObjectFromEnvName(const std::string & envName);
Rcpp::List		rcppPlotToRObject(jaspPlot * plot);
void			rcppSetUserPlotChangesFromRStateObject(jaspPlot & plot);
