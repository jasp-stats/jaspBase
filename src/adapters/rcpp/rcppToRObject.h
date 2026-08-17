#pragma once

// Dispatch for the former virtual jaspObject::toRObject(). The per-class
// implementations stay members of their (still Rcpp-based) classes until each
// class moves to core; this function reproduces the old virtual dispatch.

#include <Rcpp.h>

class jaspObject;

Rcpp::List rcppToRObject(jaspObject * obj);
