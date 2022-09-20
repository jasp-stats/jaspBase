
#include "jaspColumn.h"

setColumnDataFuncDef	jaspColumn::_jaspRCPP_setColumnDataAsScaleFunc				= nullptr;
setColumnDataFuncDef	jaspColumn::_jaspRCPP_setColumnDataAsOrdinalFunc			= nullptr;
setColumnDataFuncDef	jaspColumn::_jaspRCPP_setColumnDataAsNominalFunc			= nullptr;
setColumnDataFuncDef	jaspColumn::_jaspRCPP_setColumnDataAsNominalTextFunc		= nullptr;
getColumnTypeFuncDef	jaspColumn::_jaspRCPP_getColumnTypeFunc						= nullptr;

void jaspColumn::set_jaspRCPP_setColumnDataAsScaleFunc(setColumnDataFuncDef func)
{
	_jaspRCPP_setColumnDataAsScaleFunc = func;
}
void jaspColumn::set_jaspRCPP_setColumnDataAsScaleFuncXPtr(Rcpp::XPtr<setColumnDataFuncDef> func)
{
	set_jaspRCPP_setColumnDataAsScaleFunc(*func);
}

void jaspColumn::set_jaspRCPP_setColumnDataAsOrdinalFunc(setColumnDataFuncDef func)
{
	_jaspRCPP_setColumnDataAsScaleFunc = func;
}
void jaspColumn::set_jaspRCPP_setColumnDataAsOrdinalFuncXPtr(Rcpp::XPtr<setColumnDataFuncDef> func)
{
	set_jaspRCPP_setColumnDataAsOrdinalFunc(*func);
}

void jaspColumn::set_jaspRCPP_setColumnDataAsNominalFunc(setColumnDataFuncDef func)
{
	_jaspRCPP_setColumnDataAsNominalFunc = func;
}
void jaspColumn::set_jaspRCPP_setColumnDataAsNominalFuncXPtr(Rcpp::XPtr<setColumnDataFuncDef> func)
{
	set_jaspRCPP_setColumnDataAsNominalFunc(*func);
}

void jaspColumn::set_jaspRCPP_setColumnDataAsNominalTextFunc(setColumnDataFuncDef func)
{
	_jaspRCPP_setColumnDataAsNominalTextFunc = func;
}
void jaspColumn::set_jaspRCPP_setColumnDataAsNominalTextFuncXPtr(Rcpp::XPtr<setColumnDataFuncDef> func)
{
	set_jaspRCPP_setColumnDataAsNominalTextFunc(*func);
}

void jaspColumn::set_jaspRCPP_getColumnTypeFunc(getColumnTypeFuncDef func)
{
	_jaspRCPP_getColumnTypeFunc = func;
}
void jaspColumn::set_jaspRCPP_getColumnTypeFuncXPtr(Rcpp::XPtr<getColumnTypeFuncDef> func)
{
	set_jaspRCPP_getColumnTypeFunc(*func);
}

bool	jaspColumn::jaspRCPP_setColumnDataAsScale(			std::string columnName, Rcpp::RObject scalarData) { if (_jaspRCPP_setColumnDataAsScaleFunc			== nullptr) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false;} else return (*_jaspRCPP_setColumnDataAsScaleFunc)(columnName, scalarData); }
bool	jaspColumn::jaspRCPP_setColumnDataAsOrdinal(		std::string columnName, Rcpp::RObject scalarData) { if (_jaspRCPP_setColumnDataAsOrdinalFunc		== nullptr) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false;} else return (*_jaspRCPP_setColumnDataAsOrdinalFunc)(columnName, scalarData); }
bool	jaspColumn::jaspRCPP_setColumnDataAsNominal(		std::string columnName, Rcpp::RObject scalarData) { if (_jaspRCPP_setColumnDataAsNominalFunc		== nullptr) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false;} else return (*_jaspRCPP_setColumnDataAsNominalFunc)(columnName, scalarData); }
bool	jaspColumn::jaspRCPP_setColumnDataAsNominalText(	std::string columnName, Rcpp::RObject scalarData) { if (_jaspRCPP_setColumnDataAsNominalTextFunc	== nullptr) { jaspPrint("jaspColumn does nothing in R stand-alone!"); return false;} else return (*_jaspRCPP_setColumnDataAsNominalTextFunc)(columnName, scalarData); }

columnType jaspColumn::jaspRCPP_getColumnType(std::string columnName) { return (_jaspRCPP_getColumnTypeFunc == nullptr) ? columnType::unknown : (*_jaspRCPP_getColumnTypeFunc)(columnName); }

jaspColumn::jaspColumn(std::string columnName)
	: jaspObject(jaspObjectType::column, "jaspColumn for " + columnName)
	, _columnName(columnName)
{
	switch(jaspRCPP_getColumnType(columnName))
	{
	case columnType::scale:			_columnType = jaspColumnType::scale;		break;
	case columnType::ordinal:		_columnType = jaspColumnType::ordinal;		break;
	case columnType::nominal:		_columnType = jaspColumnType::nominal;		break;
	case columnType::nominalText:	_columnType = jaspColumnType::nominalText;	break;
	default:						_columnType = jaspColumnType::unknown;		break;
	}
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

void jaspColumn::setScale(Rcpp::RObject scalarData)
{
	_dataChanged	= jaspRCPP_setColumnDataAsScale(_columnName, scalarData);
	_typeChanged	= _columnType != jaspColumnType::scale;
	_columnType		= jaspColumnType::scale;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();
}

void jaspColumn::setOrdinal(Rcpp::RObject ordinalData)
{
	_dataChanged	= jaspRCPP_setColumnDataAsOrdinal(_columnName, ordinalData);
	_typeChanged	= _columnType != jaspColumnType::ordinal;
	_columnType		= jaspColumnType::ordinal;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();
}

void jaspColumn::setNominal(Rcpp::RObject nominalData)
{
	_dataChanged	= jaspRCPP_setColumnDataAsNominal(_columnName, nominalData);
	_typeChanged	= _columnType != jaspColumnType::nominal;
	_columnType		= jaspColumnType::nominal;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();
}

void jaspColumn::setNominalText(Rcpp::RObject nominalData)
{
	_dataChanged	= jaspRCPP_setColumnDataAsNominalText(_columnName, nominalData);
	_typeChanged	= _columnType != jaspColumnType::nominalText;
	_columnType		= jaspColumnType::nominalText;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();
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
