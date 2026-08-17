#pragma once

// Rcpp-specific conversion machinery, moved verbatim from the old jaspObject.h/.cpp
// (R-side behaviour must stay bit-identical). The functions used to be members of
// jaspObject; they are free functions now, with the per-object `_escapeHtml` flag
// passed explicitly by the callers.

#include <Rcpp.h>
#include <json/json.h>
#include <string>
#include <vector>
#include <limits>
#include "stringutils.h"

inline bool isMixedRObject(Rcpp::RObject obj) { return obj.inherits("mixed"); }

#define TO_INFINITY_AND_BEYOND																					\
{																												\
	double val = static_cast<double>(obj[row]);																	\
	return	R_IsNA(val) ? "" :																					\
				R_IsNaN(val) ? "NaN" :																			\
					val == std::numeric_limits<double>::infinity() ? "\u221E" :									\
						val == -1 * std::numeric_limits<double>::infinity() ? "-\u221E"  :						\
							Json::Value((double)(obj[row]));													\
}

template<int RTYPE>  inline Json::Value RMatrixColumnEntry_to_JsonValue(Rcpp::MatrixColumn<RTYPE> obj, int row, bool escapeHtml)	{ return ""; }

template<int RTYPE>  inline Json::Value RVectorEntry_to_JsonValue(Rcpp::Vector<RTYPE> obj, int row, bool escapeHtml)				{ return ""; }

template<> inline Json::Value RVectorEntry_to_JsonValue<INTSXP>(Rcpp::Vector<INTSXP> obj, int row, bool escapeHtml)
{
	return obj[row] == NA_INTEGER	? "" : Json::Value((int)(obj[row]));
}

template<> inline Json::Value RVectorEntry_to_JsonValue<LGLSXP>(Rcpp::Vector<LGLSXP> obj, int row, bool escapeHtml)
{
	return obj[row] == NA_LOGICAL	? "" : Json::Value((bool)(obj[row]));
}

template<> inline Json::Value RVectorEntry_to_JsonValue<STRSXP>(Rcpp::Vector<STRSXP> obj, int row, bool escapeHtml)
{
	return obj[row] == NA_STRING	? "" : Json::Value(escapeHtml ? stringUtils::escapeHtmlStuff(std::string(obj[row])) : std::string(obj[row]));
}

template<> inline Json::Value RVectorEntry_to_JsonValue<REALSXP>(Rcpp::Vector<REALSXP> obj, int row, bool escapeHtml)				TO_INFINITY_AND_BEYOND

template<> inline Json::Value RMatrixColumnEntry_to_JsonValue<INTSXP>(Rcpp::MatrixColumn<INTSXP> obj, int row, bool escapeHtml)		{ return obj[row] == NA_INTEGER	? "" : Json::Value((int)(obj[row]));			}

template<> inline Json::Value RMatrixColumnEntry_to_JsonValue<LGLSXP>(Rcpp::MatrixColumn<LGLSXP> obj, int row, bool escapeHtml)		{ return obj[row] == NA_LOGICAL	? "" : Json::Value((bool)(obj[row]));			}

template<> inline Json::Value RMatrixColumnEntry_to_JsonValue<STRSXP>(Rcpp::MatrixColumn<STRSXP> obj, int row, bool escapeHtml)		{ return obj[row] == NA_STRING	? "" : Json::Value(escapeHtml ? stringUtils::escapeHtmlStuff(std::string(obj[row])) : std::string(obj[row])); }

template<> inline Json::Value RMatrixColumnEntry_to_JsonValue<REALSXP>(Rcpp::MatrixColumn<REALSXP> obj, int row, bool escapeHtml)	TO_INFINITY_AND_BEYOND


template<int RTYPE>	 inline std::vector<Json::Value> RcppVector_to_VectorJson(Rcpp::Vector<RTYPE> obj, bool escapeHtml)
{
	std::vector<Json::Value> vec;

	for(int row=0; row<obj.size(); row++)
		vec.push_back(RVectorEntry_to_JsonValue(obj, row, escapeHtml));

	return vec;
}

template<int RTYPE>	 inline std::vector<std::vector<Json::Value>> RcppMatrix_to_Vector2Json(Rcpp::Matrix<RTYPE>	obj, bool escapeHtml)
{
	std::vector<std::vector<Json::Value>> vecvec;

	for(int col=0; col<obj.ncol(); col++)
	{
		std::vector<Json::Value> vec;

		for(int row=0; row<obj.column(col).size(); row++)
			vec.push_back(RMatrixColumnEntry_to_JsonValue(obj.column(col), row, escapeHtml));

		vecvec.push_back(vec);
	}

	return vecvec;
}

Json::Value RObject_to_JsonValue(		Rcpp::RObject	obj, bool escapeHtml);
Json::Value RObject_to_JsonValue(		Rcpp::List 		obj, bool escapeHtml);
Json::Value MixedRObject_to_JsonValue(	Rcpp::List		obj, bool escapeHtml);
std::vector<Json::Value> RList_to_VectorJson(Rcpp::List obj, bool escapeHtml);

template<int RTYPE> inline Json::Value RObject_to_JsonValue(Rcpp::Matrix<RTYPE>	obj, bool escapeHtml)
{
	Json::Value val(Json::arrayValue);

	for(int col=0; col<obj.ncol(); col++)
	{
		Json::Value valCol(Json::arrayValue);

		for(int row=0; row<obj.column(col).size(); row++)
			valCol.append(RMatrixColumnEntry_to_JsonValue(obj.column(col), row, escapeHtml));

		val.append(valCol);
	}

	return val;
}

template<int RTYPE> inline Json::Value RObject_to_JsonValue(Rcpp::Vector<RTYPE>	obj, bool escapeHtml)
{
	Json::Value val("");

	if(obj.size() == 1)
		val = RVectorEntry_to_JsonValue(obj, 0, escapeHtml);
	else if(obj.size() > 1)
	{
		val = Json::Value(Json::arrayValue);

		for(int row=0; row<obj.size(); row++)
			val.append(RVectorEntry_to_JsonValue(obj, row, escapeHtml));
	}

	return val;
}

inline std::vector<Json::Value> MixedRcppVector_to_VectorJson(Rcpp::List obj, bool escapeHtml)
{
	std::vector<Json::Value> vec;
	for(int i=0; i<obj.length(); i++)
		vec.push_back(MixedRObject_to_JsonValue(obj[i], escapeHtml));

	return vec;
}

inline std::vector<Json::Value> RcppVector_to_VectorJson(Rcpp::RObject obj, bool escapeHtml, bool throwError=false)
{
	if(Rcpp::is<Rcpp::NumericVector>(obj))			return RcppVector_to_VectorJson<REALSXP>((Rcpp::NumericVector)		obj, escapeHtml);
	else if(Rcpp::is<Rcpp::LogicalVector>(obj))		return RcppVector_to_VectorJson<LGLSXP>((Rcpp::LogicalVector)		obj, escapeHtml);
	else if(Rcpp::is<Rcpp::IntegerVector>(obj))		return RcppVector_to_VectorJson<INTSXP>((Rcpp::IntegerVector)		obj, escapeHtml);
	else if(Rcpp::is<Rcpp::StringVector>(obj))		return RcppVector_to_VectorJson<STRSXP>((Rcpp::StringVector)		obj, escapeHtml);
	else if(Rcpp::is<Rcpp::CharacterVector>(obj))	return RcppVector_to_VectorJson<STRSXP>((Rcpp::CharacterVector)		obj, escapeHtml);
	else if(isMixedRObject(obj))					return MixedRcppVector_to_VectorJson(	(Rcpp::List)				obj, escapeHtml);
	else if(Rcpp::is<Rcpp::List>(obj))				return RList_to_VectorJson((Rcpp::List)								obj, escapeHtml);
	else if(throwError) Rf_error("JASPjson::RcppVector_to_VectorJson received an SEXP that is not a Vector of some kind.");

	return std::vector<Json::Value>({""});
}

template <typename RCPP_CLASS> inline std::vector<std::string> extractElementOrColumnNames(RCPP_CLASS rObj)
{
	Rcpp::RObject colNamesRObject = Rcpp::colnames(rObj), kolnamesRObject = rObj.names();
	Rcpp::CharacterVector colNamesList;
	std::vector<std::string> colNamesVec;

	if(!colNamesRObject.isNULL() || !kolnamesRObject.isNULL())
	{
		colNamesList = !colNamesRObject.isNULL()  ? colNamesRObject : kolnamesRObject;

		for(size_t col=0; col<colNamesList.size(); col++)
			colNamesVec.push_back(Rcpp::as<std::string>(colNamesList[col]));
	}

	return colNamesVec;
}

Rcpp::DataFrame convertFactorsToCharacters(Rcpp::DataFrame df);
