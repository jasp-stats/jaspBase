#include "jaspColumn.h"
#include "jaspResults.h"

createColumnFuncDef		jaspColumn::_createColumnFunc					= nullptr;
deleteColumnFuncDef		jaspColumn::_deleteColumnFunc					= nullptr;
getColumnTypeFuncDef	jaspColumn::_getColumnTypeFunc					= nullptr;
getColumnExistsFDef		jaspColumn::_getColumnExistsFunc				= nullptr;
getColumnAnIdFuncDef	jaspColumn::_getColumnAnalysisIdFunc	 		= nullptr;
setColumnDataFuncDef	jaspColumn::_setColumnDataAsScaleFunc			= nullptr;
setColumnDataFuncDef	jaspColumn::_setColumnDataAsOrdinalFunc			= nullptr;
setColumnDataFuncDef	jaspColumn::_setColumnDataAsNominalFunc			= nullptr;
setColumnDataFuncDef	jaspColumn::_setColumnDataAsNominalTextFunc		= nullptr;
enDecodeFuncDef			jaspColumn::_encodeFunc							= nullptr;
enDecodeFuncDef			jaspColumn::_decodeFunc							= nullptr;
shouldEnDecodeFuncDef 	jaspColumn::_shouldEncodeFunc					= nullptr;
shouldEnDecodeFuncDef	jaspColumn::_shouldDecodeFunc					= nullptr;

jaspColumn::jaspColumn(std::string columnName)
	: jaspObject(jaspObjectType::column, "jaspColumn for " + columnName),
	  _columnName(columnName)
{
	if(shouldDecode(columnName))
	{
		_encoded	= columnName;
		_columnName = decode(_encoded);
		_title		= "jaspColumn for " + _columnName;
	}
	else //It isnt an encoded name of an existing column, so it should be a normal human columnname
	{
		_encoded = !getColumnExists(_columnName) ? createColumn(columnName) : encode(_columnName);
	}
	
	switch(getColumnType(_columnName))
	{
	case columnType::scale:			_columnType = jaspColumnType::scale;		break;
	case columnType::ordinal:		_columnType = jaspColumnType::ordinal;		break;
	case columnType::nominal:		_columnType = jaspColumnType::nominal;		break;
	case columnType::nominalText:	_columnType = jaspColumnType::nominalText;	break;
	default:						_columnType = jaspColumnType::unknown;		break;
	}
}

jaspColumn::jaspColumn()
	: jaspObject(jaspObjectType::column, "jaspColumn without columnName")
{
	//This one will load from JSON
}

void jaspColumn::setColumnFuncs(colDataF scalar, colDataF ordinal, colDataF nominal, colDataF nominalText, 
	colGetTF colType, colGetAIF colAnId, colCreateF colCreate, colDeleteF colDelete, colExistsF colExists,
	encDecodeF encode, encDecodeF decode, shouldEncDecodeF shouldEncode, shouldEncDecodeF shouldDecode)
{
	_createColumnFunc				= * colCreate;
	_deleteColumnFunc				= * colDelete;
	_getColumnTypeFunc 				= * colType;
	_getColumnAnalysisIdFunc		= * colAnId;
	_setColumnDataAsScaleFunc 		= * scalar;
	_setColumnDataAsOrdinalFunc 	= * ordinal;
	_setColumnDataAsNominalFunc 	= * nominal;
	_setColumnDataAsNominalTextFunc = * nominalText;
	_getColumnExistsFunc			= * colExists;
	_encodeFunc						= * encode;
	_decodeFunc						= * decode;
	_shouldEncodeFunc				= * shouldEncode;
	_shouldDecodeFunc				= * shouldDecode;
}

#define SET_COLUMN_DATA_BASE(FUNC)														\
{																						\
	if ( !FUNC || !columnIsMine(columnName))										\
	{																					\
		jaspPrint(!FUNC																	\
			? "jaspColumn does nothing in R stand-alone!"								\
			: "Column '" + columnName + "' does not belong to this analysis"); 	\
		return false;																	\
	}																					\
	else																				\
		return (*FUNC)(columnName, data);										\
}																						\

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

std::string jaspColumn::encode(const std::string & columnName)
{
	if(!_encodeFunc) 
	{
		jaspPrint("jaspColumn::encode doesnt do anything if no functions have been passed on");
		return "???";
	}
	else
		return (*_encodeFunc)(columnName); 
}

std::string jaspColumn::decode(const std::string & columnName)
{
	if(!_decodeFunc) 
	{
		jaspPrint("jaspColumn::decode doesnt do anything if no functions have been passed on");
		return "???";
	}
	else
		return (*_decodeFunc)(columnName); 
}

bool jaspColumn::shouldEncode(const std::string & columnName)
{
	if(!_shouldEncodeFunc) 
	{
		jaspPrint("jaspColumn::shouldEncode doesnt do anything if no functions have been passed on");
		return false;
	}
	else
		return (*_shouldEncodeFunc)(columnName); 
}

bool jaspColumn::shouldDecode(const std::string & columnName)
{
	if(!_shouldDecodeFunc) 
	{
		jaspPrint("jaspColumn::shouldDecode doesnt do anything if no functions have been passed on");
		return false;
	}
	else
		return (*_shouldDecodeFunc)(columnName); 
}


std::string jaspColumn::createColumn(const std::string & columnName)
{
	if(!_createColumnFunc) 
	{
		jaspPrint("jaspColumn::createColumn doesnt do anything if no functions have been passed on");
		return "";
	}
	else if(getColumnExists(columnName))
	{
		if(!columnIsMine(columnName))
			throw std::runtime_error("jaspColumn::createColumn cant create column '"+columnName+"' because it already exists, but is not created by this analysis");
		
		return encode(columnName);
	}
	else
		return (*_createColumnFunc)(columnName); 
}

/*void jaspColumn::removeFromData()
{ 
	assert(!_removed);
	
	deleteColumn(_columnName);
	_removed = true;
}*/

bool jaspColumn::deleteColumn(const std::string &columnName)
{
	if(!_deleteColumnFunc) 
	{
		jaspPrint("jaspColumn::deleteColumn doesnt do anything if no functions have been passed on");
		return "";
	}
	else if(!getColumnExists(columnName))
		throw std::runtime_error("jaspColumn::deleteColumn cant delete column '"+columnName+"' because it does not exist"); //Given that jaspColumn creates a column when you set a name this should be rather unlikely
	else if(!columnIsMine(columnName))
		throw std::runtime_error("jaspColumn::deleteColumn cant delete column '"+columnName+"' because it is not created by this analysis");
	else
		return (*_deleteColumnFunc)(columnName); 
}

Rcpp::StringVector jaspColumn::createColumnsCPP(Rcpp::StringVector columnNames)
{
	jaspPrint("jaspBase::createColumns aka jaspColumn::createColumnsCPP is deprecated. jaspColumn is all you need!");
	
	Rcpp::StringVector result;

	if(!_createColumnFunc)
	{
		jaspPrint("jaspColumn does nothing in R stand-alone!");
		return result;
	}
	
	stringvec colNames;
	colNames.reserve(columnNames.size());
	
	for(const Rcpp::String columnName : columnNames)
		colNames.push_back(columnName);

	for(const std::string & columnName : colNames)
		if(getColumnExists(columnName) && !columnIsMine(columnName))
		{
			jaspPrint("Column '"+columnName+"' already exists and does NOT belong to this analysis...");
			return result;
		}

	
	for(const std::string & columnName : colNames)
		if(!getColumnExists(columnName))
			result.push_back((*_createColumnFunc)(columnName));

	return result;
}

bool jaspColumn::setScale(Rcpp::RObject scalarData)
{
	_dataChanged	= setColumnDataAsScale(_encoded, scalarData);
	_typeChanged	= _columnType != jaspColumnType::scale;
	_columnType		= jaspColumnType::scale;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();

	return _dataChanged || _typeChanged;
}

bool jaspColumn::setOrdinal(Rcpp::RObject ordinalData)
{
	_dataChanged	= setColumnDataAsOrdinal(_encoded, ordinalData);
	_typeChanged	= _columnType != jaspColumnType::ordinal;
	_columnType		= jaspColumnType::ordinal;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();

	return _dataChanged || _typeChanged;
}

bool jaspColumn::setNominal(Rcpp::RObject nominalData)
{
	_dataChanged	= setColumnDataAsNominal(_encoded, nominalData);
	_typeChanged	= _columnType != jaspColumnType::nominal;
	_columnType		= jaspColumnType::nominal;

	if(_dataChanged || _typeChanged)
		notifyParentOfChanges();

	return _dataChanged || _typeChanged;
}

bool jaspColumn::setNominalText(Rcpp::RObject nominalData)
{
	_dataChanged	= setColumnDataAsNominalText(_encoded, nominalData);
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
	data["removed"]		= _removed;

	return data;
}


Json::Value jaspColumn::convertToJSON() const
{
	Json::Value obj		= jaspObject::convertToJSON();

	obj["columnName"]	= _columnName;
	obj["encoded"]		= _encoded;
	obj["columnType"]	= jaspColumnTypeToString(_columnType);
	

	return obj;
}

void jaspColumn::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_columnName		=							in["columnName"].asString();
	_encoded		=							in["encoded"].asString();
	_columnType		= jaspColumnTypeFromString(	in["columnType"].asString());
	_removed		= !getColumnExists(_columnName);
	_dataChanged	= false;
	_typeChanged	= false;
}

std::string jaspColumn::dataToString(std::string prefix) const
{
	std::stringstream out;

	out << prefix << "column " << _columnName << " has type " << jaspColumnTypeToString(_columnType) << " and had " << (_dataChanged? "" : "no ") << "changes!\n";

	return out.str();
}
