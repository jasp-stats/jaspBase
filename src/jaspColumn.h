#ifndef _JASPCOLUMN_HEADER
#define _JASPCOLUMN_HEADER

#include "jaspObject.h"
#include "columntype.h"

typedef bool			(*setColumnDataFuncDef)	(std::string, Rcpp::RObject);
typedef columnType		(*getColumnTypeFuncDef)	(std::string);

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

	static void			set_jaspRCPP_setColumnDataAsScaleFunc(setColumnDataFuncDef func);
	static void			set_jaspRCPP_setColumnDataAsOrdinalFunc(setColumnDataFuncDef func);
	static void			set_jaspRCPP_setColumnDataAsNominalFunc(setColumnDataFuncDef func);
	static void			set_jaspRCPP_setColumnDataAsNominalTextFunc(setColumnDataFuncDef func);
	static void			set_jaspRCPP_getColumnTypeFunc(getColumnTypeFuncDef func);

	static void			set_jaspRCPP_setColumnDataAsScaleFuncXPtr(Rcpp::XPtr<setColumnDataFuncDef> func);
	static void			set_jaspRCPP_setColumnDataAsOrdinalFuncXPtr(Rcpp::XPtr<setColumnDataFuncDef> func);
	static void			set_jaspRCPP_setColumnDataAsNominalFuncXPtr(Rcpp::XPtr<setColumnDataFuncDef> func);
	static void			set_jaspRCPP_setColumnDataAsNominalTextFuncXPtr(Rcpp::XPtr<setColumnDataFuncDef> func);
	static void			set_jaspRCPP_getColumnTypeFuncXPtr(Rcpp::XPtr<getColumnTypeFuncDef> func);


private:
	std::string		_columnName		= "";
	bool			_dataChanged	= false,
					_typeChanged	= false;
	jaspColumnType	_columnType		= jaspColumnType::unknown;

	bool			jaspRCPP_setColumnDataAsScale(			std::string columnName, Rcpp::RObject scalarData);
	bool			jaspRCPP_setColumnDataAsOrdinal(		std::string columnName, Rcpp::RObject scalarData);
	bool			jaspRCPP_setColumnDataAsNominal(		std::string columnName, Rcpp::RObject scalarData);
	bool			jaspRCPP_setColumnDataAsNominalText(	std::string columnName, Rcpp::RObject scalarData);
	columnType		jaspRCPP_getColumnType(					std::string columnName							);

	static setColumnDataFuncDef		_jaspRCPP_setColumnDataAsScaleFunc,
									_jaspRCPP_setColumnDataAsOrdinalFunc,
									_jaspRCPP_setColumnDataAsNominalFunc,
									_jaspRCPP_setColumnDataAsNominalTextFunc;
	static getColumnTypeFuncDef		_jaspRCPP_getColumnTypeFunc;



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
