#ifndef _JASPCOLUMN_HEADER
#define _JASPCOLUMN_HEADER

#include "jaspObject.h"
#include "columntype.h"
#include <Rcpp.h>

typedef bool			(*shouldEnDecodeFuncDef)	(std::string);
typedef bool			(*setColumnDataFuncDef)		(std::string, Rcpp::RObject, bool);
typedef columnType		(*getColumnTypeFuncDef)		(std::string);
typedef int				(*getColumnAnIdFuncDef)		(std::string);
typedef bool			(*getColumnExistsFDef)		(std::string);
typedef std::string		(*createColumnFuncDef)		(std::string);
typedef bool			(*deleteColumnFuncDef)		(std::string);
typedef std::string		(*enDecodeFuncDef)			(std::string);

typedef  Rcpp::XPtr<shouldEnDecodeFuncDef>			shouldEncDecodeF;
typedef  Rcpp::XPtr<setColumnDataFuncDef>			colDataF;
typedef  Rcpp::XPtr<getColumnTypeFuncDef>			colGetTF;
typedef  Rcpp::XPtr<getColumnAnIdFuncDef>			colGetAIF;
typedef  Rcpp::XPtr<createColumnFuncDef>			colCreateF;
typedef  Rcpp::XPtr<deleteColumnFuncDef>			colDeleteF;
typedef  Rcpp::XPtr<getColumnExistsFDef>			colExistsF;
typedef  Rcpp::XPtr<enDecodeFuncDef>				encDecodeF;


class jaspColumn : public jaspObject
{
public:
	jaspColumn();
	jaspColumn(std::string columnName);
	
	const std::string & nameDecoded() const { return _columnName;	}
	const std::string & nameEncoded() const { return _encoded;		}


	Json::Value		convertToJSON()								const	override;
	void			convertFromJSON_SetFields(Json::Value in)			override;
	std::string		dataToString(std::string prefix)			const	override;

	Json::Value	metaEntry()										const	override { return constructMetaEntry("column"); }
	Json::Value	dataEntry(std::string & errorMessage)			const	override;

	bool 				setScale(			Rcpp::RObject 		scalarData,		bool computed = false);
	bool 				setOrdinal(			Rcpp::RObject 		ordinalData,	bool computed = false);
	bool 				setNominal(			Rcpp::RObject		nominalData,	bool computed = false);
	bool 				setNominalText(		Rcpp::RObject 		nominalData,	bool computed = false);
	//void				removeFromData();
	static bool			columnIsMine(		const std::string & columnName); ///< "Mine" means of analysis that is running
	static bool			columnExists(		const std::string & columnName) { return getColumnExists(columnName); }
	static int			getColumnOriginalIndex(		const std::string & encodedColumnName						);

	static Rcpp::StringVector 	createColumnsCPP(Rcpp::StringVector columnNames); 		///<Checks whether the columns exist first, if not creates them otherwise does nothing. Returns a list of encoded columnNames if creation worked.


	static void			setColumnFuncs(colDataF scalar, colDataF ordinal, colDataF nominal, colGetTF colType, colGetAIF colAnaId, colGetAIF colIndex, colCreateF colCreate, colDeleteF colDelete, colExistsF colExists, encDecodeF encode, encDecodeF decode, shouldEncDecodeF shouldEncode, shouldEncDecodeF shouldDecode);
	static bool			deleteColumn(const std::string & columnName);

private:
	std::string		_columnName		= "",
					_encoded		= "";
	bool			_dataChanged	= false,
					_typeChanged	= false,
					_removed		= false;
	jaspColumnType	_columnType		= jaspColumnType::unknown;

	
	static std::string	encode(						const std::string & columnName								);
	static std::string	decode(						const std::string & columnName								);
	static bool			shouldEncode(				const std::string & columnName								);
	static bool			shouldDecode(				const std::string & columnName								);
	static std::string	createColumn(				const std::string & columnName								); ///< Returns encoded columnname
	static bool			getColumnExists(			const std::string & columnName								);
	static columnType	getColumnType(				const std::string & encodedColumnName						);
	static int			getColumnAnalysisId(		const std::string & encodedColumnName						);
	
	void				determineTypeTitle();
	bool				setColumnDataAsScale(		const std::string & encodedColumnName, Rcpp::RObject data,	bool computed=false);
	bool				setColumnDataAsOrdinal(		const std::string & encodedColumnName, Rcpp::RObject data,	bool computed=false);
	bool				setColumnDataAsNominal(		const std::string & encodedColumnName, Rcpp::RObject data,	bool computed=false);
	bool				setColumnDataAsNominalText(	const std::string & encodedColumnName, Rcpp::RObject data,	bool computed=false);
	
	static createColumnFuncDef		_createColumnFunc;
	static deleteColumnFuncDef		_deleteColumnFunc;
	static getColumnExistsFDef		_getColumnExistsFunc;
	static getColumnTypeFuncDef		_getColumnTypeFunc;
	static getColumnAnIdFuncDef		_getColumnAnalysisIdFunc,
									_getColumnOriginalIndexFunc;
	static setColumnDataFuncDef		_setColumnDataAsScaleFunc,
									_setColumnDataAsOrdinalFunc,
									_setColumnDataAsNominalFunc;
	static enDecodeFuncDef			_encodeFunc,
									_decodeFunc;
	static shouldEnDecodeFuncDef	_shouldEncodeFunc,
									_shouldDecodeFunc;
	
	
};



class jaspColumn_Interface : public jaspObject_Interface
{
public:
	jaspColumn_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	bool setScale(		Rcpp::RObject scalarData,	bool computed = false)	{ return static_cast<jaspColumn*>(myJaspObject)->setScale(scalarData,		computed);		}
	bool setOrdinal(	Rcpp::RObject ordinalData,	bool computed = false)	{ return static_cast<jaspColumn*>(myJaspObject)->setOrdinal(ordinalData,	computed);		}
	bool setNominal(	Rcpp::RObject nominalData,	bool computed = false)	{ return static_cast<jaspColumn*>(myJaspObject)->setNominal(nominalData,	computed);		}
	bool setNominalText(Rcpp::RObject nominalData,	bool computed = false)	{ return static_cast<jaspColumn*>(myJaspObject)->setNominal(nominalData,	computed);		}
	//void removeFromData()							{ return static_cast<jaspColumn*>(myJaspObject)->removeFromData();				}
};

RCPP_EXPOSED_CLASS_NODECL(jaspColumn_Interface)
#endif
