#ifndef _JASPCOLUMN_HEADER
#define _JASPCOLUMN_HEADER

#include "jaspObject.h"
#include "columntype.h"
#include <Rcpp.h>

typedef bool			(*setColumnDataFuncDef)	(std::string, Rcpp::RObject);
typedef columnType		(*getColumnTypeFuncDef)	(std::string);
typedef int				(*getColumnAnIdFuncDef)	(std::string);
typedef bool			(*getColumnExistsFDef)	(std::string);
typedef std::string		(*createColumnFuncDef)	(std::string);

typedef  Rcpp::XPtr<setColumnDataFuncDef> 	colDataF;
typedef  Rcpp::XPtr<getColumnTypeFuncDef> 	colGetTF;
typedef  Rcpp::XPtr<getColumnAnIdFuncDef> 	colGetAIF;
typedef  Rcpp::XPtr<createColumnFuncDef> 	colCreateF;
typedef  Rcpp::XPtr<getColumnExistsFDef> 	colExistsF;

class jaspColumn : public jaspObject
{
public:
	jaspColumn(std::string columnName="");


	Json::Value		convertToJSON()								const	override;
	void			convertFromJSON_SetFields(Json::Value in)			override;
	std::string		dataToString(std::string prefix)			const	override;

	Json::Value	metaEntry()										const	override { return constructMetaEntry("column"); }
	Json::Value	dataEntry(std::string & errorMessage)			const	override;


	bool 				setScale(		Rcpp::RObject 		scalarData);
	bool 				setOrdinal(		Rcpp::RObject 		ordinalData);
	bool 				setNominal(		Rcpp::RObject		nominalData);
	bool 				setNominalText(	Rcpp::RObject 		nominalData);
	bool 				columnIsMine(	const std::string & columnName);

	static Rcpp::StringVector 	createColumnsCPP(Rcpp::StringVector columnNames); 		///<Checks whether the columns exist first, if not creates them otherwise does nothing. Returns a list of encoded columnNames if creation worked.


	static void setColumnFuncs(colDataF scalar, colDataF ordinal, colDataF nominal, colDataF nominalText, colGetTF colType, colGetAIF colAnaId, colCreateF colCreate, colExistsF colExists);

private:
	std::string		_columnName		= "";
	bool			_dataChanged	= false,
					_typeChanged	= false;
	jaspColumnType	_columnType		= jaspColumnType::unknown;

	static columnType	getColumnType(				const std::string & columnName						);
	static bool			getColumnExists(			const std::string & columnName						);
	static int			getColumnAnalysisId(		const std::string & columnName						);
	bool				setColumnDataAsScale(		const std::string & columnName, Rcpp::RObject data	);
	bool				setColumnDataAsOrdinal(		const std::string & columnName, Rcpp::RObject data	);
	bool				setColumnDataAsNominal(		const std::string & columnName, Rcpp::RObject data	);
	bool				setColumnDataAsNominalText(	const std::string & columnName, Rcpp::RObject data	);
	
	static createColumnFuncDef		_createColumnFunc;
	static getColumnExistsFDef		_getColumnExistsFunc;
	static getColumnTypeFuncDef		_getColumnTypeFunc;
	static getColumnAnIdFuncDef		_getColumnAnalysisIdFunc;
	static setColumnDataFuncDef		_setColumnDataAsScaleFunc,
									_setColumnDataAsOrdinalFunc,
									_setColumnDataAsNominalFunc,
									_setColumnDataAsNominalTextFunc;
};



class jaspColumn_Interface : public jaspObject_Interface
{
public:
	jaspColumn_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	bool setScale(		Rcpp::RObject scalarData)	{ return static_cast<jaspColumn*>(myJaspObject)->setScale(scalarData);			}
	bool setOrdinal(	Rcpp::RObject ordinalData)	{ return static_cast<jaspColumn*>(myJaspObject)->setOrdinal(ordinalData);		}
	bool setNominal(	Rcpp::RObject nominalData)	{ return static_cast<jaspColumn*>(myJaspObject)->setNominal(nominalData);		}
	bool setNominalText(Rcpp::RObject nominalData)	{ return static_cast<jaspColumn*>(myJaspObject)->setNominalText(nominalData);	}
};

RCPP_EXPOSED_CLASS_NODECL(jaspColumn_Interface)
#endif
