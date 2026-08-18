#ifndef _JASPCOLUMN_HEADER
#define _JASPCOLUMN_HEADER

// CORE (R-free) version of jaspColumn. The column-data callbacks use
// std::any payloads so that any host (R, Python, …) can pass opaque data.
// The R adapter (rcppColumn.h) provides the XPtr-based setColumnFuncs that
// the desktop engine expects, bridging to these plain function pointers.

#include "jaspObject.h"
#include "columntype.h"
#include <any>

typedef bool			(*shouldEnDecodeFuncDef)	(std::string);
typedef bool			(*setColumnDataFuncDef)		(std::string, const std::any &, bool);
typedef columnType		(*getColumnTypeFuncDef)		(std::string);
typedef int				(*getColumnAnIdFuncDef)		(std::string);
typedef bool			(*getColumnExistsFDef)		(std::string);
typedef std::string		(*createColumnFuncDef)		(std::string, bool);
typedef bool			(*deleteColumnFuncDef)		(std::string);
typedef std::string		(*enDecodeFuncDef)			(std::string);

// Plain aliases so the core API stays R-free; the R adapter wraps these in
// Rcpp::XPtr when registering setColumnFuncs with the module.
typedef shouldEnDecodeFuncDef	shouldEncDecodeF;
typedef setColumnDataFuncDef	colDataF;
typedef getColumnTypeFuncDef	colGetTF;
typedef getColumnAnIdFuncDef	colGetAIF;
typedef createColumnFuncDef		colCreateF;
typedef deleteColumnFuncDef		colDeleteF;
typedef getColumnExistsFDef		colExistsF;
typedef enDecodeFuncDef			encDecodeF;


class jaspColumn : public jaspObject
{
public:
	jaspColumn();
	jaspColumn(std::string columnName, bool computed=false);
	
	const std::string & nameDecoded() const { return _columnName;	}
	const std::string & nameEncoded() const { return _encoded;		}


	Json::Value		convertToJSON()								const	override;
	void			convertFromJSON_SetFields(Json::Value in)			override;
	std::string		dataToString(std::string prefix)			const	override;

	Json::Value	metaEntry()										const	override { return constructMetaEntry("column"); }
	Json::Value	dataEntry(std::string & errorMessage)			const	override;

	bool 				setScale(			std::any scalarData,		bool computed = false);
	bool 				setOrdinal(			std::any ordinalData,	bool computed = false);
	bool 				setNominal(			std::any nominalData,	bool computed = false);
	bool 				setNominalText(		std::any nominalData,	bool computed = false);
	//void				removeFromData();
	static bool			columnIsMine(		const std::string & columnName); ///< "Mine" means of analysis that is running
	static bool			columnExists(		const std::string & columnName) { return getColumnExists(columnName); }
	static int			getColumnOriginalIndex(		const std::string & encodedColumnName						);

	static std::vector<std::string> createColumns(const std::vector<std::string> & columnNames);

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
	static std::string	createColumn(				const std::string & columnName,		bool computed=false		); ///< Returns encoded columnname
	static bool			getColumnExists(			const std::string & columnName								);
	static columnType	getColumnType(				const std::string & encodedColumnName						);
	static int			getColumnAnalysisId(		const std::string & encodedColumnName						);
	
	void				determineTypeTitle();
	bool				setColumnDataAsScale(		const std::string & encodedColumnName, const std::any & data,	bool computed=false);
	bool				setColumnDataAsOrdinal(		const std::string & encodedColumnName, const std::any & data,	bool computed=false);
	bool				setColumnDataAsNominal(		const std::string & encodedColumnName, const std::any & data,	bool computed=false);
	bool				setColumnDataAsNominalText(	const std::string & encodedColumnName, const std::any & data,	bool computed=false);
	
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

#endif
