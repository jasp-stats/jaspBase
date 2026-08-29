#pragma once

// CORE (R-free) version of jaspTable.h. SEXP/ingest dispatch, name extraction
// from R attributes, toRObject() and the *_Interface live under
// src/adapters/rcpp/ (rcppTableIngest + rcppInterfaces). Storage is
// std::vector<std::vector<Json::Value>> exactly as in the original, so the
// JSON/formatting/footnotes machinery is unchanged.

#include "jaspObject.h"
#include "jaspList.h"
#include <functional>

struct jaspColRowCombination
{
	jaspColRowCombination(std::string name, std::string title, bool overwrite, bool removeSeparator, Json::Value colNames, Json::Value rowNames, Json::Value colOvertitles, Json::Value rowOvertitles)
		: name(name), title(title), overwrite(overwrite), removeSeparator(removeSeparator), colNames(colNames), rowNames(rowNames), colOvertitles(colOvertitles), rowOvertitles(rowOvertitles) {}

	jaspColRowCombination(Json::Value convertFromThis) { throw std::runtime_error("Not implemented");}

	std::string name, title;
	bool overwrite, removeSeparator;
	Json::Value colNames, rowNames, colOvertitles, rowOvertitles;

	std::string toString();

	Json::Value convertToJSON() const { throw std::runtime_error("Not implemented"); }

};

namespace footnotesNamespace
{

struct tableFields
{

	tableFields(std::set<Json::Value> rows, std::set<Json::Value> cols) : _rows(rows), _cols(cols) {}

	Json::Value rowsToJSON()				const;
	Json::Value colsToJSON()				const;

	struct hasher //Special hash func obj to differentiate between different sets of tableFields
	{
		std::size_t operator()(tableFields const & tf) const noexcept
		{
			return std::hash<std::string>{}(tf.getCompareString());
		}
	};

	struct comparer
	{
		bool operator()(const tableFields & lhs, const tableFields & rhs) const
		{
			return lhs.getCompareString() < rhs.getCompareString(); //Don't really care about the results logic
		}

	};

	std::string getCompareString()	const { return rowsToJSON().toStyledString() + "<$>" + colsToJSON().toStyledString(); }

private:
	std::set<Json::Value>	_rows,
							_cols;
};

inline bool operator==(const tableFields & lhs, const tableFields & rhs)
{
	return lhs.getCompareString() == rhs.getCompareString();
}

struct footnotes
{
	void		insert(std::string text, std::string symbol, std::vector<Json::Value> colNames, std::vector<Json::Value> rowNames);
	void		convertFromJSON_SetFields(Json::Value footnotes);
	Json::Value	convertToJSON() const;
	void		convertToJSONOrdered(std::map<std::string, size_t> rowNames, std::map<std::string, size_t> colNames, Json::Value & fullList, Json::Value & mergedList) const;

	std::map<std::string, std::map<std::string, std::set<tableFields, tableFields::comparer> >> _data; //text -> symbol -> rows+cols  (public so the R adapter can build toRObject)
};

}

using footnotesNamespace::footnotes;

///Neutral ingest payload: column-major cells plus optional column/row names.
///Host adapters (R SEXP, Python dict/DataFrame) build this and call
///setDataColumns().
struct jaspTableData
{
	std::vector<std::vector<Json::Value>>	columns;	//First columns, then rows, like _data
	std::vector<std::string>				colNames,	//"" allowed, becomes col<i>
											rowNames;	//"" allowed, becomes row<i>
};

class jaspTable : public jaspObject
{
public:
	jaspTable(std::string title = "") : jaspObject(jaspObjectType::table, title), _colNames("colNames"), _colTypes("colTypes"), _colTitles("colTitles"), _colOvertitles("colOvertitles"), _colFormats("colFormats"), _rowNames("rowNames"), _rowTitles("rowTitles") {}

	void			setColNames(		std::vector<std::string> newNames,		const std::map<std::string, std::string> & fields = {})	{ _colNames.setRows(newNames, fields); }
	jaspStringlist	_colNames;

	void			setColTypes(		std::vector<std::string> newTypes,		const std::map<std::string, std::string> & fields = {})	{ _colTypes.setRows(newTypes, fields); }
	jaspStringlist	_colTypes;

	void			setColTitles(		std::vector<std::string> newTitles,		const std::map<std::string, std::string> & fields = {})	{ _colTitles.setRows(newTitles, fields); }
	jaspStringlist	_colTitles;

	void			setColOvertitles(	std::vector<std::string> newTitles,		const std::map<std::string, std::string> & fields = {})	{ _colOvertitles.setRows(newTitles, fields); }
	jaspStringlist	_colOvertitles;

	void			setColFormats(		std::vector<std::string> newFormats,	const std::map<std::string, std::string> & fields = {})	{ _colFormats.setRows(newFormats, fields); }
	jaspStringlist	_colFormats;

	void			setColCombines(		std::vector<bool> newCombines,			const std::map<std::string, bool> & fields = {})			{ _colCombines.setRows(newCombines, fields); }
	jaspBoollist	_colCombines;

	void			setRowNames(		std::vector<std::string> newNames,		const std::map<std::string, std::string> & fields = {})	{ _rowNames.setRows(newNames, fields); }
	jaspStringlist	_rowNames;

	void			setRowTitles(		std::vector<std::string> newTitles,		const std::map<std::string, std::string> & fields = {})	{ _rowTitles.setRows(newTitles, fields); }
	jaspStringlist	_rowTitles;

	///Neutral ingest: clear existing data and load columns (+ any names) from
	///jaspTableData. R/Python adapters convert their native data first.
	void		setDataColumns(const jaspTableData & newData);

	void		addFootnote(std::string message, std::string symbol, std::vector<Json::Value> col_names, std::vector<Json::Value> row_names);

	///neutral addColumnInfo: name="" means "use default colN"; title/type/format/overtitle "" mean "leave unset"; hasCombine gates the combine bool.
	void		addColumnInfo(std::string name, std::string title, std::string type, std::string format, bool hasCombine, bool combine, std::string overtitle);

	std::string dataToString(std::string prefix)		const	override;

	void		complete()	override	{ if(_status == "running") _status = "complete"; }
	void		letRun()	override	{ _status = "running"; }

	bool		canShowErrorMessage()					const	override { return true; }

	Json::Value	metaEntry()								const	override { return constructMetaEntry("table"); }
	Json::Value	dataEntry(std::string & errorMessage)	const	override;
	std::string	toHtml()								const	override;

	std::string defaultColName(size_t col)	const	{ return "col"+ std::to_string(col); }
	std::string defaultRowName(size_t row)	const	{ return "row"+ std::to_string(row); }
	std::string	getRowName(size_t row)		const	{ return _rowNames[row] == "" ? defaultRowName(row) : _rowNames[row]; }
	std::string getColName(size_t col)		const	{ return _colNames[col] == "" ? defaultColName(col) : _colNames[col]; }
	std::string getColType(size_t col)		const;

	bool		isSpecialColumn(size_t col)			const;
	bool		columnSpecified(size_t col)			const { return _specifiedColumns.count(getColName(col)) > 0;	}
	bool		columnSpecified(std::string col)	const { return _specifiedColumns.count(col) > 0;				}

	Json::Value	getCell(			size_t col, size_t row, size_t maxCol, size_t maxRow) const;
	std::string	getCellFormatted(	size_t col, size_t row, size_t maxCol, size_t maxRow) const;

	void		calculateMaxColRow(size_t & maxCol, size_t & maxRow) const;

	void		setExpectedSize(size_t columns, size_t rows)	{ setExpectedRows(rows); setExpectedColumns(columns);	}
	void		setExpectedRows(size_t rows)					{ _expectedRowCount = rows;								}
	void		setExpectedColumns(size_t columns)				{ _expectedColumnCount = columns;						}

protected:
	std::vector<std::string>	getDisplayableColTitles(bool normalizeLengths = true, bool onlySpecifiedColumns = true)		const;
	std::vector<std::string>	getDisplayableRowTitles(bool normalizeLengths = true)										const;
	void						rectangularDataWithNamesToString(	std::stringstream & out, std::string prefix,	std::vector<std::vector<std::string>> vierkant, std::vector<std::string> sideNames, std::vector<std::string> topNames, std::map<std::string,std::string> sideOvertitles, std::map<std::string,std::string> topOvertitles) const;
	void						rectangularDataWithNamesToHtml(		std::stringstream & out,						std::vector<std::vector<std::string>> vierkant, std::vector<std::string> sideNames, std::vector<std::string> topNames, std::map<std::string,std::string> sideOvertitles, std::map<std::string,std::string> topOvertitles) const;


			std::map<std::string, std::string>				getOvertitlesMap()																					const;
			std::vector<std::vector<std::string>>			dataToRectangularVector(bool normalizeColLengths = false, bool normalizeRowLengths = false)			const;
	static	std::vector<std::vector<std::string>>			transposeRectangularVector(const std::vector<std::vector<std::string>> & in);
			std::map<std::string, std::map<size_t, size_t>> getOvertitleRanges(std::vector<std::string> names, std::map<std::string,std::string> overtitles)	const;

	int getDesiredColumnIndexFromNameForRowAdding(std::string colName, int previouslyAddedUnnamed);

	Json::Value	schemaJson(Json::Value tmpFootnotesFull)	const;
	Json::Value	rowsJson(Json::Value tmpFootnotesFull)		const;

public:
	jaspTableColumnType deriveColumnType(int col)			const;

protected:

	std::map<std::string, size_t> mapColNamesToIndices()	const;
	std::map<std::string, size_t> mapRowNamesToIndices()	const;

	Json::Value convertToJSON()								const	override;
	void		convertFromJSON_SetFields(Json::Value in)			override;

	bool isMixedJson(const Json::Value &v) const { return v.isObject() && !v.get("value", Json::nullValue).isNull() && !v.get("type", Json::nullValue).isNull() && v.isMember("format"); }

public:
	// cell-storage primitives, public so host adapters (R SEXP dispatch,
	// Python conversions) can build tables on top of them:
	int		getDesiredColumnIndexFromNameForColumnAdding(std::string colName);
	void	addOrSetColumnInData(std::vector<Json::Value> column, std::string colName="");
	int		pushbackToColumnInData(std::vector<Json::Value> column, std::string colName, int equalizedColumnsLength, int previouslyAddedUnnamed);
	void	setColumnCellsAt(std::vector<Json::Value> column, size_t col);
	void	setRowNamesWhereApplicable(std::vector<std::string> rowNamesList);
	int		equalizeColumnsLengths();

	bool					_transposeTable = false,
							_transposeWithOvertitle = false,
							_showSpecifiedColumnsOnly = false;
	std::string				_status = "running";

	std::set<std::string>	_specifiedColumns;

	//public so host adapters (R toRObject/mixed-columns, Python) can read/write the raw cells
	footnotes 								_footnotes;
	std::vector<std::vector<Json::Value>>	_data;	//First columns, then rows.

private:
	std::vector<jaspColRowCombination>		_colRowCombinations;
	size_t									_expectedColumnCount	= 0,
											_expectedRowCount		= 0;
};
