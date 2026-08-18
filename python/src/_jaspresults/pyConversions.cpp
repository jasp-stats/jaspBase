#include "pyConversions.h"

#include <cmath>
#include <limits>
#include <sstream>
#include "stringutils.h" // stringUtils::escapeHtmlStuff (Common, header-only)

namespace py = pybind11;

// ---------------------------------------------------------------------------
// optional numpy / pandas introspection. Both are optional dependencies; when
// absent the type checks simply return false so plain Python containers keep
// working. Cached module handles are leaked on purpose (destroying a static
// py::object at interpreter shutdown deadlocks).
// ---------------------------------------------------------------------------
static const py::object * pyModuleOrNull(const char * name)
{
	static std::map<std::string, const py::object *> cache;
	auto found = cache.find(name);
	if(found != cache.end())
		return found->second;

	const py::object * mod = nullptr;
	try
	{
		mod = new py::object(py::module_::import(name));
	}
	catch(const py::error_already_set &)
	{
		mod = new py::object(py::none());
	}
	cache.emplace(name, mod);
	return mod;
}

static bool pyIsInstanceOfModule(const py::handle & h, const char * modName, const char * clsName)
{
	if(h.is_none())
		return false;

	const py::object * mod = pyModuleOrNull(modName);
	if(mod->is_none())
		return false;

	try
	{
		return py::isinstance(h, mod->attr(clsName));
	}
	catch(const py::error_already_set &)
	{
		return false;
	}
}

bool pyIsDataFrame(const py::handle & h)	{ return pyIsInstanceOfModule(h, "pandas", "DataFrame"); }
bool pyIsSeries(const py::handle & h)		{ return pyIsInstanceOfModule(h, "pandas", "Series"); }
bool pyIsNdArray(const py::handle & h)		{ return pyIsInstanceOfModule(h, "numpy", "ndarray");	}
bool pyIsCategorical(const py::handle & h)	{ return pyIsInstanceOfModule(h, "pandas", "Categorical");	}

static bool pyIsNA(const py::handle & h)
{
	if(h.is_none())
		return true;

	//pandas.isna covers pd.NA / pd.NaT / np.nan scalars. ndarrays return an
	//array (not a bool), so the isinstance<bool> guard keeps them out here.
	const py::object * pd = pyModuleOrNull("pandas");
	if(pd->is_none())
		return false;

	try
	{
		py::object res = pd->attr("isna")(h);
		return py::isinstance<py::bool_>(res) && res.cast<bool>();
	}
	catch(const py::error_already_set &)
	{
		return false;
	}
}

// ---------------------------------------------------------------------------
// §3.1 scalar -> cell
// ---------------------------------------------------------------------------
Json::Value pyCellToJsonValue(const py::handle & h, bool escapeHtml)
{
	//None / pd.NA / pd.NaT -> empty cell (R NA -> "")
	if(pyIsNA(h))
		return Json::Value("");

	//explicit "NaN" marker (R's NaN cell; Python can't otherwise ask for it)
	if(py::isinstance<pyNaNString>(h))
		return Json::Value("NaN");

	//bool before int: in Python bools are ints
	if(py::isinstance<py::bool_>(h))
		return Json::Value(h.cast<bool>());

	if(py::isinstance<py::int_>(h))
		return Json::Value(static_cast<Json::Int64>(h.cast<long long>()));

	if(py::isinstance<py::float_>(h))
	{
		double v = h.cast<double>();
		if(std::isnan(v))																					return Json::Value("");
		if(v ==  std::numeric_limits<double>::infinity())													return Json::Value("\u221E");
		if(v == -std::numeric_limits<double>::infinity())													return Json::Value("-\u221E");
		return Json::Value(v);
	}

	if(py::isinstance<py::str>(h))
	{
		std::string s = h.cast<std::string>();
		return escapeHtml ? Json::Value(stringUtils::escapeHtmlStuff(s)) : Json::Value(s);
	}

	//numpy scalars (np.int64, np.float64, np.bool_, np.str_, ...) -> unwrap to builtins
	if(py::hasattr(h, "item") && !py::isinstance<py::dict>(h) && !py::isinstance<py::list>(h))
	{
		try
		{
			return pyCellToJsonValue(h.attr("item")(), escapeHtml);
		}
		catch(const py::error_already_set &)
		{
			//fall through to the generic error below
		}
	}

	throw std::runtime_error("jaspresults: cannot convert this Python value to a table cell");
}

Json::Value pyMixedPartToJsonValue(const py::handle & h, bool escapeHtml)
{
	if(h.is_none())
		return Json::nullValue;

	return pyCellToJsonValue(h, escapeHtml);
}

///A mixed cell is a dict with (at least) value/type/format keys, mirroring the
///R `list(value=, type=, format=)` object with class "mixed".
static bool pyIsMixedDict(const py::dict & d)
{
	return d.contains("value") && d.contains("type") && d.contains("format");
}

///Mixed cells can appear inside pySequenceToCells, so handle dict there too.
///Generic dicts/lists become JSON objects/arrays as single cells, mirroring
///RObject_to_JsonValue on R lists.
Json::Value pyCellOrMixedToJsonValue(const py::handle & h, bool escapeHtml)
{
	if(py::isinstance<py::dict>(h))
	{
		py::dict d = h.cast<py::dict>();
		if(pyIsMixedDict(d))
		{
			Json::Value mixed(Json::objectValue);
			mixed["value"]	= pyMixedPartToJsonValue(d["value"],	escapeHtml);
			mixed["type"]	= pyMixedPartToJsonValue(d["type"],		escapeHtml);
			mixed["format"]	= pyMixedPartToJsonValue(d["format"],	escapeHtml);
			return mixed;
		}

		Json::Value obj(Json::objectValue);
		for(auto item : d)
			obj[py::str(item.first).cast<std::string>()] = pyCellOrMixedToJsonValue(item.second, escapeHtml);
		return obj;
	}

	if(py::isinstance<py::list>(h) || py::isinstance<py::tuple>(h))
	{
		Json::Value arr(Json::arrayValue);
		for(auto item : h)
			arr.append(pyCellOrMixedToJsonValue(item, escapeHtml));
		return arr;
	}

	return pyCellToJsonValue(h, escapeHtml);
}

// ---------------------------------------------------------------------------
// materialise a Python sequence into a vector of cells
// ---------------------------------------------------------------------------
static bool pyIsSequenceLike(const py::handle & h)
{
	if(py::isinstance<py::list>(h) || py::isinstance<py::tuple>(h))
		return true;
	if(pyIsNdArray(h) || pyIsSeries(h) || pyIsCategorical(h))
		return true;
	if(py::isinstance<py::str>(h) || py::isinstance<py::bytes>(h) || py::isinstance<py::dict>(h))
		return false;

	//range, generators and other iterables
	try
	{
		return py::isinstance(h, py::module_::import("collections.abc").attr("Iterable"));
	}
	catch(const py::error_already_set &)
	{
		return false;
	}
}

static py::object pyMaterialiseSequence(const py::handle & h)
{
	//numpy ndarray / pandas Series -> native Python list
	if(pyIsNdArray(h) || pyIsSeries(h))
		return h.attr("tolist")();

	//tuple / list are already fine
	if(py::isinstance<py::list>(h) || py::isinstance<py::tuple>(h))
		return py::reinterpret_borrow<py::object>(h);

	//range / generator / Categorical / other iterables
	return py::reinterpret_steal<py::object>(PySequence_List(h.ptr()));
}

std::vector<Json::Value> pySequenceToCells(const py::handle & h, bool escapeHtml)
{
	std::vector<Json::Value> cells;

	if(h.is_none())
		return cells;

	//scalars (incl. strings, which must NOT be iterated char-by-char) become a
	//single cell, mirroring R's length-1 columns
	if(!pyIsSequenceLike(h))
		return {pyCellOrMixedToJsonValue(h, escapeHtml)};

	py::object seq = pyMaterialiseSequence(h);
	for(auto item : seq)
		cells.push_back(pyCellOrMixedToJsonValue(item, escapeHtml));

	return cells;
}

std::vector<std::string> pyToStringVector(const py::handle & h)
{
	std::vector<std::string> out;

	if(h.is_none())
		return out;

	if(!pyIsSequenceLike(h))
		return {py::str(h).cast<std::string>()};

	py::object seq = pyMaterialiseSequence(h);
	for(auto item : seq)
		out.push_back(item.is_none() ? "" : py::str(item).cast<std::string>());

	return out;
}

std::pair<std::vector<std::string>, std::map<std::string, std::string>> pyToStringRowsAndFields(const py::handle & h)
{
	std::vector<std::string>				rows;
	std::map<std::string, std::string>		fields;

	if(h.is_none())
		return {rows, fields};

	if(py::isinstance<py::dict>(h))
	{
		py::dict d = h.cast<py::dict>();
		for(auto item : d)
		{
			std::string key		= py::str(item.first).cast<std::string>();
			std::string value	= item.second.is_none() ? "" : py::str(item.second).cast<std::string>();
			rows.push_back(value);
			if(key != "")
				fields[key] = value;
		}
		return {rows, fields};
	}

	rows = pyToStringVector(h);
	return {rows, fields};
}

std::pair<std::vector<bool>, std::map<std::string, bool>> pyToBoolRowsAndFields(const py::handle & h)
{
	std::vector<bool>			rows;
	std::map<std::string, bool>	fields;

	if(h.is_none())
		return {rows, fields};

	if(py::isinstance<py::dict>(h))
	{
		py::dict d = h.cast<py::dict>();
		for(auto item : d)
		{
			std::string key	= py::str(item.first).cast<std::string>();
			bool value		= py::cast<bool>(item.second);
			rows.push_back(value);
			if(key != "")
				fields[key] = value;
		}
		return {rows, fields};
	}

	py::object seq = pyMaterialiseSequence(h);
	for(auto item : seq)
		rows.push_back(py::cast<bool>(item));

	return {rows, fields};
}

// ---------------------------------------------------------------------------
// §3.2 ingest dispatch
//
// Python rules (documented in the plan §3.2), which deliberately match R's
// data-orientation conventions:
//   dict {str: sequence} / pandas DataFrame -> COLUMNS (like R data.frame)
//   pandas Series                           -> one named COLUMN
//   tuple/list of scalars                   -> one ROW (like R atomic vector)
//   tuple/list of sequences                 -> ROWS (like R list of rows)
//   2-D np.ndarray                          -> COLUMNS (like R matrix)
//   1-D np.ndarray                          -> one ROW (like R atomic vector)
// ---------------------------------------------------------------------------

/// dict {str: sequence} (or DataFrame) -> column-major cells + names.
static void pyMappingToColumns(jaspTable * table, const py::handle & data, std::vector<std::vector<Json::Value>> & columns, std::vector<std::string> & colNames, bool escapeHtml)
{
	if(pyIsDataFrame(data))
	{
		py::list cols = data.attr("columns").cast<py::list>();
		for(auto c : cols)
		{
			std::string name = py::str(c).cast<std::string>();
			colNames.push_back(name);
			columns.push_back(pySequenceToCells(data[py::reinterpret_borrow<py::object>(c)], escapeHtml));
		}
		return;
	}

	//plain dict
	py::dict d = data.cast<py::dict>();
	for(auto item : d)
	{
		colNames.push_back(py::str(item.first).cast<std::string>());
		columns.push_back(pySequenceToCells(item.second, escapeHtml));
	}
}

void pyIngestSetData(jaspTable * table, py::object data, py::object colNamesObj, py::object rowNamesObj)
{
	bool escapeHtml = table->getEscapeHtml();

	if(data.is_none())
	{
		table->_data.clear();
		//still apply explicit names if given
		if(!colNamesObj.is_none())
			table->setColNames(pyToStringVector(colNamesObj));
		if(!rowNamesObj.is_none())
			table->setRowNames(pyToStringVector(rowNamesObj));
		table->notifyParentOfChanges();
		return;
	}

	jaspTableData d;

	if(pyIsDataFrame(data) || py::isinstance<py::dict>(data))
	{
		pyMappingToColumns(table, data, d.columns, d.colNames, escapeHtml);
		if(!colNamesObj.is_none())
			d.colNames = pyToStringVector(colNamesObj);
		if(!rowNamesObj.is_none())
			d.rowNames = pyToStringVector(rowNamesObj);
		table->setDataColumns(d);
		table->notifyParentOfChanges();
		return;
	}

	if(pyIsSeries(data))
	{
		std::string name = "";
		if(!data.attr("name").is_none())
			name = py::str(data.attr("name")).cast<std::string>();

		d.colNames	= {name};
		d.columns	= {pySequenceToCells(data, escapeHtml)};
		if(!rowNamesObj.is_none())
			d.rowNames = pyToStringVector(rowNamesObj);
		table->setDataColumns(d);
		table->notifyParentOfChanges();
		return;
	}

	//numpy ndarray: 2-D -> COLUMNS (like an R matrix), 1-D -> one ROW (like an
	//R atomic vector). Optional col_names/row_names override the (absent) names.
	if(pyIsNdArray(data))
	{
		int ndim = data.attr("ndim").cast<int>();

		if(ndim >= 2)
		{
			//column-major: transpose the row-major tolist() output
			py::list rowsList = data.attr("tolist")().cast<py::list>();
			size_t nRows = rowsList.size();
			size_t nCols = nRows > 0 ? rowsList[0].cast<py::list>().size() : 0;

			d.columns.assign(nCols, std::vector<Json::Value>());
			for(size_t r = 0; r < nRows; r++)
			{
				py::list row = rowsList[r].cast<py::list>();
				for(size_t c = 0; c < nCols && c < row.size(); c++)
					d.columns[c].push_back(pyCellOrMixedToJsonValue(row[c], escapeHtml));
			}

			if(!colNamesObj.is_none())
				d.colNames = pyToStringVector(colNamesObj);
			if(!rowNamesObj.is_none())
				d.rowNames = pyToStringVector(rowNamesObj);
			table->setDataColumns(d);
			table->notifyParentOfChanges();
			return;
		}
		//1-D falls through to the generic sequence handling below (one row)
	}

	//a flat sequence of scalars == one ROW (like an R atomic vector via addRow)
	//a sequence of sequences == ROWS
	//We detect "is the first non-None element itself a sequence?" to decide.
	py::object seq = pyMaterialiseSequence(data);

	bool isFirstElementScalar = true;
	bool anyElementSequence	= false;
	for(auto item : seq)
	{
		if(py::isinstance<py::list>(item) || py::isinstance<py::tuple>(item) || pyIsNdArray(item) || pyIsSeries(item))
		{
			isFirstElementScalar	= false;
			anyElementSequence		= true;
			break;
		}
		//only inspect the first non-None element to classify the shape
		if(!item.is_none())
		{
			isFirstElementScalar = true;
			break;
		}
	}

	std::vector<std::string> rowNames = rowNamesObj.is_none() ? std::vector<std::string>() : pyToStringVector(rowNamesObj);

	if(!anyElementSequence)
	{
		//one row: each scalar becomes one column? No — R addRow semantics: the
		//vector's cells map across existing columns. For a brand-new table this
		//is a single row of N cells. Build it by appending to columns 0..N-1.
		std::vector<Json::Value> rowCells = pySequenceToCells(seq, escapeHtml);

		//optional explicit column names for the row's cells
		std::vector<std::string> cellColNames = colNamesObj.is_none() ? std::vector<std::string>() : pyToStringVector(colNamesObj);

		table->_data.clear();
		for(size_t i = 0; i < rowCells.size(); i++)
		{
			std::string colName = (i < cellColNames.size() && cellColNames[i] != "") ? cellColNames[i] : "";
			table->addOrSetColumnInData(std::vector<Json::Value>({rowCells[i]}), colName);
		}

		if(rowNames.size() > 0)
			table->_rowNames[0] = rowNames[0];

		table->notifyParentOfChanges();
		return;
	}

	//ROWS: sequence of sequences (or dicts). Mirror addRows for a fresh table.
	int equalizedColumnsLength = table->equalizeColumnsLengths();
	int previouslyAddedUnnamed = 0;

	for(size_t row = 0; row < rowNames.size(); row++)
		table->_rowNames[row + equalizedColumnsLength] = rowNames[row];

	std::vector<std::string> cellColNames = colNamesObj.is_none() ? std::vector<std::string>() : pyToStringVector(colNamesObj);

	for(auto sub : seq)
	{
		if(py::isinstance<py::dict>(sub))
		{
			//dict row: keys name the columns, values are single cells
			//(like R addRows(list(list(a=1, b=2))))
			py::dict d = sub.cast<py::dict>();
			for(auto item : d)
			{
				std::string colName = py::str(item.first).cast<std::string>();
				previouslyAddedUnnamed = table->pushbackToColumnInData(std::vector<Json::Value>({pyCellOrMixedToJsonValue(item.second, escapeHtml)}), colName, equalizedColumnsLength, previouslyAddedUnnamed);
			}
		}
		else
		{
			std::vector<Json::Value> rowCells = pySequenceToCells(sub, escapeHtml);
			for(size_t i = 0; i < rowCells.size(); i++)
			{
				std::string colName = (i < cellColNames.size() && cellColNames[i] != "") ? cellColNames[i] : "";
				previouslyAddedUnnamed = table->pushbackToColumnInData(std::vector<Json::Value>({rowCells[i]}), colName, equalizedColumnsLength, previouslyAddedUnnamed);
			}
		}

		equalizedColumnsLength = table->equalizeColumnsLengths();
	}

	table->notifyParentOfChanges();
}

void pyIngestSetColumn(jaspTable * table, std::string columnName, py::object column)
{
	int colIndex = table->getDesiredColumnIndexFromNameForColumnAdding(columnName);

	std::vector<Json::Value> cells = pySequenceToCells(column, table->getEscapeHtml());
	table->setColumnCellsAt(cells, colIndex);

	table->notifyParentOfChanges();
}

void pyIngestAddColumns(jaspTable * table, py::object data)
{
	if(data.is_none())
		return;

	bool escapeHtml = table->getEscapeHtml();

	if(pyIsDataFrame(data) || py::isinstance<py::dict>(data))
	{
		std::vector<std::vector<Json::Value>>	columns;
		std::vector<std::string>				colNames;
		pyMappingToColumns(table, data, columns, colNames, escapeHtml);

		for(size_t col = 0; col < columns.size(); col++)
			table->addOrSetColumnInData(columns[col], col < colNames.size() ? colNames[col] : "");
	}
	else
	{
		//single sequence -> one column
		table->_data.push_back(pySequenceToCells(data, escapeHtml));
	}

	table->notifyParentOfChanges();
}

void pyIngestAddRows(jaspTable * table, py::object data, std::vector<std::string> rowNames)
{
	if(data.is_none())
		return;

	bool escapeHtml = table->getEscapeHtml();

	int equalizedColumnsLength		= table->equalizeColumnsLengths();
	int previouslyAddedUnnamed		= 0;

	for(size_t row = 0; row < rowNames.size(); row++)
		table->_rowNames[row + equalizedColumnsLength] = rowNames[row];

	if(pyIsDataFrame(data) || py::isinstance<py::dict>(data))
	{
		//DataFrame/dict interpreted as rows: each mapping entry contributes one
		//row of cells across the columns identified by its key.
		std::vector<std::vector<Json::Value>>	columns;
		std::vector<std::string>				colNames;
		pyMappingToColumns(table, data, columns, colNames, escapeHtml);

		for(size_t col = 0; col < columns.size(); col++)
			previouslyAddedUnnamed = table->pushbackToColumnInData(columns[col], col < colNames.size() ? colNames[col] : "", equalizedColumnsLength, previouslyAddedUnnamed);
	}
	else
	{
		py::object seq = pyMaterialiseSequence(data);
		for(auto sub : seq)
		{
			std::vector<Json::Value> rowCells = pySequenceToCells(sub, escapeHtml);
			for(size_t i = 0; i < rowCells.size(); i++)
				previouslyAddedUnnamed = table->pushbackToColumnInData(std::vector<Json::Value>({rowCells[i]}), "", equalizedColumnsLength, previouslyAddedUnnamed);

			equalizedColumnsLength = table->equalizeColumnsLengths();
		}
	}

	table->notifyParentOfChanges();
}

void pyIngestAddRow(jaspTable * table, py::object row, std::string rowName)
{
	if(row.is_none())
		return;

	bool escapeHtml = table->getEscapeHtml();

	int equalizedColumnsLength	= table->equalizeColumnsLengths();
	int previouslyAddedUnnamed	= 0;

	if(rowName != "")
		table->_rowNames[equalizedColumnsLength] = rowName;

	if(py::isinstance<py::dict>(row))
	{
		//dict row: keys name the columns, each value is a single cell
		//(mirrors R addRow(named list))
		py::dict d = row.cast<py::dict>();
		for(auto item : d)
		{
			std::string colName = py::str(item.first).cast<std::string>();
			Json::Value cell = pyCellOrMixedToJsonValue(item.second, escapeHtml);
			previouslyAddedUnnamed = table->pushbackToColumnInData(std::vector<Json::Value>({cell}), colName, equalizedColumnsLength, previouslyAddedUnnamed);
		}
	}
	else
	{
		std::vector<Json::Value> cells = pySequenceToCells(row, escapeHtml);
		for(size_t i = 0; i < cells.size(); i++)
			previouslyAddedUnnamed = table->pushbackToColumnInData(std::vector<Json::Value>({cells[i]}), "", equalizedColumnsLength, previouslyAddedUnnamed);
	}

	table->notifyParentOfChanges();
}

void pyIngestAddColumnInfo(jaspTable * table, py::object name, py::object title, py::object type, py::object format, py::object combine, py::object overtitle)
{
	std::string colName = name.is_none() ? table->defaultColName(table->_colNames.rowCount()) : py::str(name).cast<std::string>();
	table->_specifiedColumns.insert(colName);

	table->_colNames.add(colName);

	std::string lastAddedColName = table->getColName(table->_colNames.rowCount() - 1);

	if(!title.is_none())		table->_colTitles[		lastAddedColName ] = py::str(title)		.cast<std::string>();
	if(!type.is_none())			table->_colTypes[		lastAddedColName ] = py::str(type)		.cast<std::string>();
	if(!format.is_none())		table->_colFormats[		lastAddedColName ] = py::str(format)	.cast<std::string>();
	if(!overtitle.is_none())	table->_colOvertitles[	lastAddedColName ] = py::str(overtitle)	.cast<std::string>();
	if(!combine.is_none())		table->_colCombines[	lastAddedColName ] = py::cast<bool>(combine);
}

void pyIngestAddFootnote(jaspTable * table, std::string message, py::object symbol, py::object colNames, py::object rowNames)
{
	if(message == "")
		throw std::runtime_error("One would expect a footnote to at least contain a message..");

	std::string strSymbol = symbol.is_none() ? "" : py::str(symbol).cast<std::string>();

	bool escapeHtml = table->getEscapeHtml();
	std::vector<Json::Value> cols = colNames.is_none() ? std::vector<Json::Value>() : pySequenceToCells(colNames, escapeHtml);
	std::vector<Json::Value> rows = rowNames.is_none() ? std::vector<Json::Value>() : pySequenceToCells(rowNames, escapeHtml);

	table->addFootnote(message, strSymbol, cols, rows);
}
