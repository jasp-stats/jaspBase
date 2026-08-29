#pragma once

// R-backed jaspContainer helpers: the original insert dispatch, wrapJaspObject,
// list-construction, at() and toRObject that depend on Rcpp types. See
// src/core/jaspContainer.h for the R-free tree/admin logic.

#include <Rcpp.h>

class jaspContainer;
class jaspObject;

void			rcppContainerInsert(jaspContainer * container, std::string field, Rcpp::RObject value);
jaspContainer *	rcppContainerFromList(Rcpp::List convertThis);
Rcpp::RObject	rcppWrapJaspObject(jaspObject * ref);
Rcpp::RObject	rcppContainerAt(jaspContainer * container, std::string field);
Rcpp::List		rcppContainerToRObject(jaspContainer * container);
