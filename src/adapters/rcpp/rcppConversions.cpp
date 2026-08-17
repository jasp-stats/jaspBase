// Implementations moved verbatim from the old src/jaspObject.cpp (behaviour must
// stay bit-identical), with the per-object `_escapeHtml` flag passed explicitly.

#include "rcppConversions.h"

std::vector<Json::Value> RList_to_VectorJson(Rcpp::List obj, bool escapeHtml)
{
	std::vector<Json::Value> vec;

	for(int row=0; row<obj.size(); row++)
		vec.push_back(RObject_to_JsonValue((Rcpp::RObject)obj[row], escapeHtml));

	return vec;
}

Json::Value RObject_to_JsonValue(Rcpp::RObject obj, bool escapeHtml)
{
	if(obj.isNULL())								return Json::nullValue;
	else if(isMixedRObject(obj))					return MixedRObject_to_JsonValue((Rcpp::List)				obj, escapeHtml);
	else if(Rcpp::is<Rcpp::List>(obj))				return RObject_to_JsonValue((Rcpp::List)					obj, escapeHtml);
	else if(Rcpp::is<Rcpp::DataFrame>(obj))			return RObject_to_JsonValue((Rcpp::List)					obj, escapeHtml);
	else if(Rcpp::is<Rcpp::NumericMatrix>(obj))		return RObject_to_JsonValue<REALSXP>((Rcpp::NumericMatrix)	obj, escapeHtml);
	else if(Rcpp::is<Rcpp::NumericVector>(obj))		return RObject_to_JsonValue<REALSXP>((Rcpp::NumericVector)	obj, escapeHtml);
	else if(Rcpp::is<Rcpp::IntegerVector>(obj))		return RObject_to_JsonValue<INTSXP>((Rcpp::IntegerVector)	obj, escapeHtml);
	else if(Rcpp::is<Rcpp::LogicalVector>(obj))		return RObject_to_JsonValue<LGLSXP>((Rcpp::LogicalVector)	obj, escapeHtml);
	else if(Rcpp::is<Rcpp::CharacterVector>(obj))	return RObject_to_JsonValue<STRSXP>((Rcpp::CharacterVector)	obj, escapeHtml);
	else if(Rcpp::is<Rcpp::StringVector>(obj))		return RObject_to_JsonValue<STRSXP>((Rcpp::StringVector)	obj, escapeHtml);
	else if(obj.isS4())								return "an S4, which is too complicated for jaspResults now.";
	else											return "something that is not understood by jaspResults right now..";
}

Json::Value MixedRObject_to_JsonValue(Rcpp::List obj, bool escapeHtml)
{

	Json::Value value(Json::objectValue);

	// sometimes we receive list(mixed) and sometimes mixed, ideally we always just get mixed but I'm not sure that's possible with addRows.
	Rcpp::List data = obj.length() != 3 ? obj[0] : obj;

	value["value"]  = RObject_to_JsonValue((Rcpp::RObject)data["value"], escapeHtml);
	value["type"]   = RObject_to_JsonValue((Rcpp::RObject)data["type"], escapeHtml);
	value["format"] = RObject_to_JsonValue((Rcpp::RObject)data["format"], escapeHtml);
	

	return value;

}


Json::Value RObject_to_JsonValue(Rcpp::List obj, bool escapeHtml)
{
	bool atLeastOneNamed = false;

	Rcpp::RObject namesListRObject = obj.names();
	Rcpp::CharacterVector namesList;

	if(!namesListRObject.isNULL())
	{
		namesList = namesListRObject;

		for(int row=0; row<obj.size(); row++)
			if(namesList[row] != "")
				atLeastOneNamed = true;
	}

	Json::Value val = atLeastOneNamed ? Json::objectValue : Json::arrayValue;

	if(atLeastOneNamed)
		for(int row=obj.size() - 1; row>=0; row--) //We go backwards because in R the first entry of a name in a list is used. So to emulate this we go backwars and we override an earlier occurence. (aka you have two elements with the name "a" in a list and in R list$a returns the first occurence. This is now also the element visible in the json.)
		{
			std::string name(namesList[row]);

			if(name == "")
				name = "element_" + std::to_string(row);

			val[name] = RObject_to_JsonValue((Rcpp::RObject)obj[row], escapeHtml);
		}
	else
		for(int row=0; row<obj.size(); row++)
			val.append(RObject_to_JsonValue((Rcpp::RObject)obj[row], escapeHtml));


	return val;
}

Rcpp::DataFrame convertFactorsToCharacters(Rcpp::DataFrame df)
{

	for(int col=0; col<df.length(); col++)
		if(Rf_isFactor(df[col]))
		{
			Rcpp::IntegerVector		originalColumn	= df[col];

			Rcpp::CharacterVector	factorLevels	= originalColumn.attr("levels");

/*#ifdef JASP_DEBUG
			//In ifdef because we dont really have access to log here.
			std::cout	<< "converting factors to characters for dataframe\n"
						<< "originalColumn: " << originalColumn << "\n"
						<< "factorLevels: " << factorLevels << std::endl;
#endif*/

			Rcpp::CharacterVector	charCol(originalColumn.size());

			for(int i=0; i<originalColumn.size(); i++)
				if(originalColumn[i] > 0) //it can be INT_MIN at least, but if we are doing a -1 on it anyhow it should just be bigger than 0
					charCol[i] = factorLevels[originalColumn[i] - 1];

			df[col] = charCol;
		}

	return df;
}
