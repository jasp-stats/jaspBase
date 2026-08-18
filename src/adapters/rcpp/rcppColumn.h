#pragma once

// R-facing column-callback registration, moved from the old src/jaspColumn.h.
// The desktop engine hands us Rcpp::XPtr-wrapped function pointers (the data
// setters take raw Rcpp::RObject payloads); we unwrap them and bridge the three
// data setters into the R-free core signatures (std::any payloads). The module
// registration keeps the exact same names and signatures as before.

#include <Rcpp.h>
#include "jaspColumn.h"

typedef bool (*rcppSetColumnDataFuncDef)(std::string, Rcpp::RObject, bool);

void				rcppSetColumnFuncs(	Rcpp::XPtr<rcppSetColumnDataFuncDef>	scalar,
										Rcpp::XPtr<rcppSetColumnDataFuncDef>	ordinal,
										Rcpp::XPtr<rcppSetColumnDataFuncDef>	nominal,
										Rcpp::XPtr<getColumnTypeFuncDef>		colType,
										Rcpp::XPtr<getColumnAnIdFuncDef>		colAnaId,
										Rcpp::XPtr<getColumnAnIdFuncDef>		colIndex,
										Rcpp::XPtr<createColumnFuncDef>			colCreate,
										Rcpp::XPtr<deleteColumnFuncDef>			colDelete,
										Rcpp::XPtr<getColumnExistsFDef>			colExists,
										Rcpp::XPtr<enDecodeFuncDef>				encode,
										Rcpp::XPtr<enDecodeFuncDef>				decode,
										Rcpp::XPtr<shouldEnDecodeFuncDef>		shouldEncode,
										Rcpp::XPtr<shouldEnDecodeFuncDef>		shouldDecode);

Rcpp::StringVector	rcppCreateColumnsCPP(Rcpp::StringVector columnNames);
