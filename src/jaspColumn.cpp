#include "jaspColumn.h"
#include "jaspResults.h"

createColumnFuncDef		jaspColumn::_createColumnFunc					= nullptr;
getColumnTypeFuncDef	jaspColumn::_getColumnTypeFunc					= nullptr;
getColumnExistsFDef		jaspColumn::_getColumnExistsFunc				= nullptr;
getColumnAnIdFuncDef	jaspColumn::_getColumnAnalysisIdFunc	 		= nullptr;
setColumnDataFuncDef	jaspColumn::_setColumnDataAsScaleFunc			= nullptr;
setColumnDataFuncDef	jaspColumn::_setColumnDataAsOrdinalFunc			= nullptr;
setColumnDataFuncDef	jaspColumn::_setColumnDataAsNominalFunc			= nullptr;
setColumnDataFuncDef	jaspColumn::_setColumnDataAsNominalTextFunc		= nullptr;

jaspColumn::jaspColumn(std::string columnName)
	: jaspObject(jaspObjectType::column, "jaspColumn for " + columnName)
	, _columnName(columnName)
{
	switch(getColumnType(columnName))
	{
	case columnType::scale:			_columnType = jaspColumnType::scale;		break;
	case columnType::ordinal:		_columnType = jaspColumnType::ordinal;		break;
	case columnType::nominal:		_columnType = jaspColumnType::nominal;		break;
	case columnType::nominalText:	_columnType = jaspColumnType::nominalText;	break;
	default:						_columnType = jaspColumnType::unknown;		break;
	}
}


void jaspColumn::setColumnFuncs(colDataF scalar, colDataF ordinal, colDataF nominal, colDataF nominalText, 
	colGetTF colType, colGetAIF colAnId, colCreateF colCreate, colExistsF colExists)
{
	_createColumnFunc				= * colCreate;
	_getColumnTypeFunc 				= * colType;
	_getColumnAnalysisIdFunc		= * colAnId;
	_setColumnDataAsScaleFunc 		= * scalar;
	_setColumnDataAsOrdinalFunc 	= * ordinal;
	_setColumnDataAsNominalFunc 	= * nominal;
	_setColumnDataAsNominalTextFunc = * nominalText;
	_getColumnExistsFunc			= * colExists;
}

#define SET_COLUMN_DATA_BASE(FUNC)												\
{																				\
	if ( !FUNC || !columnIsMine(columnName))									\
	{																			\
		jaspPrint(!FUNC															\
			? "jaspColumn does nothing in R stand-alone!"						\
			: "Column '" + columnName + "' does not belong to this analysis"); 	\
		return false;															\
	} 																			\
	else 																		\
		return (*FUNC)(columnName, data);										\
}																				\

bool	jaspColumn::setColumnDataAsScale(		const std::string & columnName, Rcpp::RObject data) SET_COLUMN_DATA_BASE(_setColumnDataAsScaleFunc)
bool	jaspColumn::setColumnDataAsOrdinal(		const std::string & columnName, Rcpp::RObject data) SET_COLUMN_DATA_BASE(_setColumnDataAsOrdinalFunc)
bool	jaspColumn::setColumnDataAsNominal(		const std::string & columnName, Rcpp::RObject data) SET_COLUMN_DATA_BASE(_setColumnDataAsNominalFunc)
bool	jaspColumn::setColumnDataAsNominalText(	const std::string & columnName, Rcpp::RObject data) SET_COLUMN_DATA_BASE(_setColumnDataAsNominalTextFunc)

columnType jaspColumn::getColumnType(const std::string & columnName)
{ 
	if(!_getColumnTypeFunc) 
		return columnType::unknown;
	else
		return (*_getColumnTypeFunc)(columnName); 
}

int jaspColumn::getColumnAnalysisId(const std::string & columnName)
{
	if(!_getColumnAnalysisIdFunc) 
		return -1;
	else
		return (*_getColumnAnalysisIdFunc)(columnName); 
}

bool jaspColumn::columnIsMine(	const std::string & columnName)
{
	if(jaspResults::analysisId() == -1)
		return true;

	//jaspPrint("jaspColumn::columnIsMine?\njaspResults::analysisId(): " + std::to_string(jaspResults::analysisId()));
	//jaspPrint("getColumnAnalysisId("+columnName+"): " + std::to_string(getColumnAnalysisId(columnName)));

	return jaspResults::analysisId() == getColumnAnalysisId(columnName);
}

bool jaspColumn::getColumnExists(const std::string & columnName)
{
	if(!_getColumnExistsFunc) 
	{
		jaspPrint("jaspColumn::getColumnExists doesnt do anything if no functions have been passed on");
		return false;
	}
	else
		return (*_getColumnExistsFunc)(columnName); 
}

Rcpp::StringVector jaspColumn::createColumnsCPP(Rcpp::StringVector columnNames)
{
	Rcpp::StringVector result;

	if(!_createColumnFunc)
	{
		jaspPrint("jaspColumn does nothing in R stand-alone!");
		return result;
	}

	for(const Rcpp::String columnName : columnNames)
		if(getColumnExists(columnName))
			return result;

	
	for(const Rcpp::String columnName : columnNames)
		result.push_back((*_createColumnFunc)(columnName));

	return result;
}

bool jaspColumn::setScale(Rcpp::RObject scalarData)
{
	_dataChanged	= setColumnDataAsScale(_columnName, scalarData);
	_typeChanged	= _columnType != jaspColumnType::scale;
	_columnType		= jaspColumnType::scale;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();

	return _dataChanged || _typeChanged;
}

bool jaspColumn::setOrdinal(Rcpp::RObject ordinalData)
{
	_dataChanged	= setColumnDataAsOrdinal(_columnName, ordinalData);
	_typeChanged	= _columnType != jaspColumnType::ordinal;
	_columnType		= jaspColumnType::ordinal;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();

	return _dataChanged || _typeChanged;
}

bool jaspColumn::setNominal(Rcpp::RObject nominalData)
{
	_dataChanged	= setColumnDataAsNominal(_columnName, nominalData);
	_typeChanged	= _columnType != jaspColumnType::nominal;
	_columnType		= jaspColumnType::nominal;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();

	return _dataChanged || _typeChanged;
}

bool jaspColumn::setNominalText(Rcpp::RObject nominalData)
{
	_dataChanged	= setColumnDataAsNominalText(_columnName, nominalData);
	_typeChanged	= _columnType != jaspColumnType::nominalText;
	_columnType		= jaspColumnType::nominalText;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();

	return _dataChanged || _typeChanged;
}


Json::Value jaspColumn::dataEntry(std::string & errorMessage) const
{
	Json::Value data(jaspObject::dataEntry(errorMessage));

	data["columnName"]	= _columnName;
	data["columnType"]	= jaspColumnTypeToString(_columnType);
	data["dataChanged"]	= _dataChanged;
	data["typeChanged"]	= _typeChanged;

	return data;
}


Json::Value jaspColumn::convertToJSON() const
{
	Json::Value obj		= jaspObject::convertToJSON();

	obj["columnName"]	= _columnName;
	obj["columnType"]	= jaspColumnTypeToString(_columnType);

	return obj;
}

void jaspColumn::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_columnName = in["columnName"].asString();
	_columnType	= jaspColumnTypeFromString(in["columnType"].asString());
	_dataChanged	= false;
}

std::string jaspColumn::dataToString(std::string prefix) const
{
	std::stringstream out;

	out << prefix << "column " << _columnName << " has type " << jaspColumnTypeToString(_columnType) << " and had " << (_dataChanged? "" : "no ") << "changes!\n";

	return out.str();
}
