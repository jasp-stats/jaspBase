// R-facing column-callback registration; bridges the Rcpp::XPtr function
// pointers from the desktop engine into the R-free core jaspColumn callbacks.
// The three column-data setters keep their Rcpp::RObject payload end-to-end:
// the core passes it through opaquely as std::any.

#include "rcppColumn.h"

static rcppSetColumnDataFuncDef _rcppSetColumnDataAsScaleFunc		= nullptr,
								_rcppSetColumnDataAsOrdinalFunc		= nullptr,
								_rcppSetColumnDataAsNominalFunc		= nullptr;

static bool rcppSetColumnDataAsScaleBridge(std::string columnName, const std::any & data, bool computed)
{
	try
	{
		return (*_rcppSetColumnDataAsScaleFunc)(columnName, std::any_cast<Rcpp::RObject>(data), computed);
	}
	catch(const std::bad_any_cast &)
	{
		return false;
	}
}

static bool rcppSetColumnDataAsOrdinalBridge(std::string columnName, const std::any & data, bool computed)
{
	try
	{
		return (*_rcppSetColumnDataAsOrdinalFunc)(columnName, std::any_cast<Rcpp::RObject>(data), computed);
	}
	catch(const std::bad_any_cast &)
	{
		return false;
	}
}

static bool rcppSetColumnDataAsNominalBridge(std::string columnName, const std::any & data, bool computed)
{
	try
	{
		return (*_rcppSetColumnDataAsNominalFunc)(columnName, std::any_cast<Rcpp::RObject>(data), computed);
	}
	catch(const std::bad_any_cast &)
	{
		return false;
	}
}

void rcppSetColumnFuncs(	Rcpp::XPtr<rcppSetColumnDataFuncDef>	scalar,
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
							Rcpp::XPtr<shouldEnDecodeFuncDef>		shouldDecode)
{
	_rcppSetColumnDataAsScaleFunc	= *scalar;
	_rcppSetColumnDataAsOrdinalFunc	= *ordinal;
	_rcppSetColumnDataAsNominalFunc	= *nominal;

	jaspColumn::setColumnFuncs(	rcppSetColumnDataAsScaleBridge,
								rcppSetColumnDataAsOrdinalBridge,
								rcppSetColumnDataAsNominalBridge,
								*colType, *colAnaId, *colIndex, *colCreate, *colDelete, *colExists,
								*encode, *decode, *shouldEncode, *shouldDecode);
}

Rcpp::StringVector rcppCreateColumnsCPP(Rcpp::StringVector columnNames)
{
	Rcpp::StringVector result;

	std::vector<std::string> created = jaspColumn::createColumns(Rcpp::as<std::vector<std::string>>(columnNames));

	for(const std::string & encodedName : created)
		result.push_back(encodedName);

	return result;
}
