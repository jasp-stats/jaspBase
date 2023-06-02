#ifndef _JASPCOLUMN_HEADER
#define _JASPCOLUMN_HEADER

#include "jaspObject.h"
#include "columntype.h"
#include <Rcpp.h>

typedef bool			(*setColumnDataFuncDef)	(std::string, Rcpp::RObject);
typedef columnType		(*getColumnTypeFuncDef)	(std::string);

typedef  Rcpp::XPtr<setColumnDataFuncDef> colDataF;
typedef  Rcpp::XPtr<getColumnTypeFuncDef> colgetTF;

class jaspColumn : public jaspObject
{
public:
	jaspColumn(std::string columnName="");


	Json::Value		convertToJSON()								const	override;
	void			convertFromJSON_SetFields(Json::Value in)			override;
	std::string		dataToString(std::string prefix)			const	override;

	Json::Value	metaEntry()										const	override { return constructMetaEntry("column"); }
	Json::Value	dataEntry(std::string & errorMessage)			const	override;


	void setScale(		Rcpp::RObject scalarData);
	void setOrdinal(	Rcpp::RObject ordinalData);
	void setNominal(	Rcpp::RObject nominalData);
	void setNominalText(Rcpp::RObject nominalData);


	static void setColumnFuncs(colDataF scalar, colDataF ordinal, colDataF nominal, colDataF nominalText, colgetTF colType);

private:
	std::string		_columnName		= "";
	bool			_dataChanged	= false,
					_typeChanged	= false;
	jaspColumnType	_columnType		= jaspColumnType::unknown;

	bool			setColumnDataAsScale(		const std::string & columnName, Rcpp::RObject data);
	bool			setColumnDataAsOrdinal(		const std::string & columnName, Rcpp::RObject data);
	bool			setColumnDataAsNominal(		const std::string & columnName, Rcpp::RObject data);
	bool			setColumnDataAsNominalText(	const std::string & columnName, Rcpp::RObject data);
	columnType		getColumnType(				const std::string & columnName							);

	static setColumnDataFuncDef		_setColumnDataAsScaleFunc,
									_setColumnDataAsOrdinalFunc,
									_setColumnDataAsNominalFunc,
									_setColumnDataAsNominalTextFunc;
	static getColumnTypeFuncDef		_getColumnTypeFunc;



};



class jaspColumn_Interface : public jaspObject_Interface
{
public:
	jaspColumn_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void setScale(		Rcpp::RObject scalarData)	{ static_cast<jaspColumn*>(myJaspObject)->setScale(scalarData);			}
	void setOrdinal(	Rcpp::RObject ordinalData)	{ static_cast<jaspColumn*>(myJaspObject)->setOrdinal(ordinalData);		}
	void setNominal(	Rcpp::RObject nominalData)	{ static_cast<jaspColumn*>(myJaspObject)->setNominal(nominalData);		}
	void setNominalText(Rcpp::RObject nominalData)	{ static_cast<jaspColumn*>(myJaspObject)->setNominalText(nominalData);	}
};

RCPP_EXPOSED_CLASS_NODECL(jaspColumn_Interface)
#endif
