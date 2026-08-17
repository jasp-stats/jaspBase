// R-backed jaspTable ingest & export, moved verbatim from the old
// src/jaspTable.{h,cpp}: the SEXP dispatch and R-attribute name extraction
// that feed the neutral cell storage in src/core/jaspTable.h, plus the
// NULL-based addColumnInfo/addFootnote and toRObject() (incl. mixed columns).

#include "rcppTableIngest.h"
#include "jaspTable.h"
#include "rcppConversions.h"
#include "rcppInterfaces.h"

static size_t rcppLengthFromList(Rcpp::List list)				{ return list.size();	}
template<int RTYPE> static size_t rcppLengthFromVector(Rcpp::Vector<RTYPE> vec)		{ return vec.size();	}

static size_t rcppLengthFromRObject(Rcpp::RObject rObj)
{
	if(rObj.isNULL())								return 0;
	else if(Rcpp::is<Rcpp::List>(rObj))				return rcppLengthFromList((Rcpp::List)						rObj);
	else if(Rcpp::is<Rcpp::NumericVector>(rObj))	return rcppLengthFromVector<REALSXP>((Rcpp::NumericVector)	rObj);
	else if(Rcpp::is<Rcpp::LogicalVector>(rObj))	return rcppLengthFromVector<LGLSXP>((Rcpp::LogicalVector)	rObj);
	else if(Rcpp::is<Rcpp::IntegerVector>(rObj))	return rcppLengthFromVector<INTSXP>((Rcpp::IntegerVector)	rObj);
	else if(Rcpp::is<Rcpp::StringVector>(rObj))		return rcppLengthFromVector<STRSXP>((Rcpp::StringVector)	rObj);
	else if(Rcpp::is<Rcpp::CharacterVector>(rObj))	return rcppLengthFromVector<STRSXP>((Rcpp::CharacterVector)	rObj);
	else Rf_error("Unexpected type..");

	return 0;

}

template <typename RCPP_CLASS> static std::vector<std::string> rcppExtractRowNames(jaspTable * table, RCPP_CLASS rObj, bool setRowNamesInTable=false)
{
	Rcpp::RObject rowNamesRObject = Rcpp::rownames(rObj), rijnamesRObject = rObj.attr("row.names");
	Rcpp::CharacterVector rowNamesList;
	std::vector<std::string> rowNamesVec;

	if(!rowNamesRObject.isNULL() || !rijnamesRObject.isNULL())
	{
		rowNamesList = !rowNamesRObject.isNULL()  ? rowNamesRObject : rijnamesRObject;

		for(size_t row=0; row<rowNamesList.size(); row++)
		{
			rowNamesVec.push_back(Rcpp::as<std::string>(rowNamesList[row]));

			if(setRowNamesInTable && rowNamesList[row] != "" && (table->_rowNames.rowCount() <= row || table->_rowNames[row] == "")) //Add new rowNames or overwrite unset ones but if the user took the trouble to manually set it then just leave it I guess?
				table->_rowNames[row] = rowNamesList[row];
		}
	}

	return rowNamesVec;
}

template<int RTYPE>	static void rcppSetDataFromVector(jaspTable * table, Rcpp::Vector<RTYPE> newData)
{
	std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
	rcppExtractRowNames(table, newData, true);

	table->_data.clear();
	auto cols = RcppVector_to_VectorJson<RTYPE>(newData, table->getEscapeHtml());

	for(int col=0; col<cols.size(); col++)
		table->addOrSetColumnInData(std::vector<Json::Value>({cols[col]}), localColNames.size() > col ? localColNames[col] : "");
}

static void rcppSetDataFromList(jaspTable * table, Rcpp::List newData)
{
	std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
	rcppExtractRowNames(table, newData, true);

	table->_data.clear();
	for(size_t col=0; col<newData.size(); col++)
		table->addOrSetColumnInData(RcppVector_to_VectorJson((Rcpp::RObject)newData[col], table->getEscapeHtml()), localColNames.size() > col ? localColNames[col] : "");
}

template<int RTYPE> static void rcppSetDataFromMatrix(jaspTable * table, Rcpp::Matrix<RTYPE> newData)
{
	std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
	rcppExtractRowNames(table, newData, true);

	std::vector<std::vector<Json::Value>> jsonMat = RcppMatrix_to_Vector2Json<RTYPE>(newData, table->getEscapeHtml());

	table->_data.clear();
	for(size_t col=0; col<jsonMat.size(); col++)
		table->addOrSetColumnInData(jsonMat[col], localColNames.size() > col ? localColNames[col] : "");
}

template<int RTYPE>	static void rcppAddColumnFromVector(jaspTable * table, Rcpp::Vector<RTYPE> newData)
{
	table->setRowNamesWhereApplicable(extractElementOrColumnNames(newData));

	table->_data.push_back(RcppVector_to_VectorJson<RTYPE>(newData, table->getEscapeHtml()));
}

template<int RTYPE>	static void rcppSetColumnFromVector(jaspTable * table, Rcpp::Vector<RTYPE> newData, size_t col)
{
	table->setRowNamesWhereApplicable(extractElementOrColumnNames(newData));

	table->setColumnCellsAt(RcppVector_to_VectorJson<RTYPE>(newData, table->getEscapeHtml()), col);
}

static void rcppSetColumnFromMixedVector(jaspTable * table, Rcpp::List newData, size_t col)
{
	table->setRowNamesWhereApplicable(extractElementOrColumnNames(newData));

	table->setColumnCellsAt(MixedRcppVector_to_VectorJson(newData, table->getEscapeHtml()), col);
}

static void rcppSetColumnFromList(jaspTable * table, Rcpp::List column, int colIndex)
{
	std::vector<std::string> localRowNames = extractElementOrColumnNames(column);
	table->setRowNamesWhereApplicable(localRowNames);

	std::vector<Json::Value> cells;
	for(int row=0; row<column.size(); row++)
	{
		std::vector<Json::Value> jsonVec = RcppVector_to_VectorJson((Rcpp::RObject)column[row], table->getEscapeHtml(), false);
		cells.push_back(jsonVec.size() > 0 ? jsonVec[0u] : Json::nullValue);
	}

	table->setColumnCellsAt(cells, colIndex);
}

template<int RTYPE>	static void rcppAddColumnsFromMatrix(jaspTable * table, Rcpp::Matrix<RTYPE> newData)
{
	std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
	rcppExtractRowNames(table, newData, true);

	std::vector<std::vector<Json::Value>> jsonMat = RcppMatrix_to_Vector2Json<RTYPE>(newData, table->getEscapeHtml());

	for(size_t col=0; col<jsonMat.size(); col++)
		table->addOrSetColumnInData(jsonMat[col], localColNames.size() > col ? localColNames[col] : "");
}

static void rcppAddColumnsFromList(jaspTable * table, Rcpp::List newData)
{
	size_t elementLenghts = 0;
	for(int el=0; el<newData.size(); el++)
		elementLenghts = std::max(rcppLengthFromRObject((Rcpp::RObject)newData[el]), elementLenghts);

	if(elementLenghts <= 1 && newData.size() > 1) //each entry is 1 or 0, this must be a single row with columnnames and not a set of rows with rownames..
	{
		Rcpp::List newColList;
		auto shield = new Rcpp::Shield<Rcpp::List>(newColList);
		newColList.push_back(newData);
		rcppAddColumnsFromList(table, newColList);
		delete shield;

		return;
	}

	std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
	rcppExtractRowNames(table, newData, true);

	for(int col=0; col<newData.size(); col++)
		table->addOrSetColumnInData(RcppVector_to_VectorJson((Rcpp::RObject)newData[col], table->getEscapeHtml(), false), localColNames.size() > col ? localColNames[col] : "");
}

template<int RTYPE>	static void rcppAddRowFromVector(jaspTable * table, Rcpp::Vector<RTYPE> newData, Rcpp::CharacterVector newRowNames)
{
	std::vector<std::string> localColNames = extractElementOrColumnNames(newData);

	auto row = RcppVector_to_VectorJson<RTYPE>(newData, table->getEscapeHtml());

	int equalizedColumnsLength = table->equalizeColumnsLengths();
	int previouslyAddedUnnamedCols = 0;

	for(int row=0; row<newRowNames.size(); row++)
		table->_rowNames[row + equalizedColumnsLength] = newRowNames[row];

	for(int col=0; col<row.size(); col++)
		previouslyAddedUnnamedCols = table->pushbackToColumnInData(std::vector<Json::Value>({row[col]}), localColNames.size() > col ? localColNames[col] : "", equalizedColumnsLength, previouslyAddedUnnamedCols);

}

static void rcppAddRowsFromList(jaspTable * table, Rcpp::List newData, Rcpp::CharacterVector newRowNames)
{
	int equalizedColumnsLength		= table->equalizeColumnsLengths(),
		previouslyAddedUnnamedCols	= 0;

	std::vector<std::string> localRowNames = extractElementOrColumnNames(newData);

	for(size_t row=0; row<localRowNames.size(); row++)
		table->_rowNames[row + equalizedColumnsLength] = localRowNames[row];

	for(size_t row=0; row<newRowNames.size(); row++)
		table->_rowNames[row + equalizedColumnsLength] = newRowNames[row];

	for(size_t row=0; row<newData.size(); row++)
	{
		Rcpp::RObject rij = (Rcpp::RObject)newData[row];
		std::vector<std::string> localColNames;

		if(Rcpp::is<Rcpp::List>(rij))
			localColNames = extractElementOrColumnNames<Rcpp::List>(Rcpp::as<Rcpp::List>(rij));

		auto jsonRij = RcppVector_to_VectorJson(rij, table->getEscapeHtml());

		for(size_t col=0; col<jsonRij.size(); col++)
			previouslyAddedUnnamedCols	= table->pushbackToColumnInData(std::vector<Json::Value>({jsonRij[col]}), localColNames.size() > col ? localColNames[col] : "", equalizedColumnsLength, previouslyAddedUnnamedCols);

		equalizedColumnsLength = table->equalizeColumnsLengths();
	}
}

static void rcppAddRowFromList(jaspTable * table, Rcpp::List newData, Rcpp::CharacterVector newRowNames)
{
	Rcpp::List newRowList;
	auto shield = new Rcpp::Shield<Rcpp::List>(newRowList);
	newRowList.push_back(newData);
	rcppAddRowsFromList(table, newRowList, newRowNames);
	delete shield;
}

static void rcppAddRowsFromDataFrame(jaspTable * table, Rcpp::DataFrame newData)
{
	newData							= convertFactorsToCharacters(newData);
	int equalizedColumnsLength		= table->equalizeColumnsLengths();
	int previouslyAddedUnnamedCols	= 0;

	std::vector<std::string> localColNames = extractElementOrColumnNames(newData);

	for(size_t col=0; col<newData.size(); col++)
	{
		Rcpp::RObject kolom			= (Rcpp::RObject)newData[col];
		auto jsonKolom				= RcppVector_to_VectorJson(kolom, table->getEscapeHtml());
		previouslyAddedUnnamedCols	= table->pushbackToColumnInData(jsonKolom, localColNames.size() > col ? localColNames[col] : "", equalizedColumnsLength, previouslyAddedUnnamedCols);
	}

}

template<int RTYPE>	static void rcppAddRowsFromMatrix(jaspTable * table, Rcpp::Matrix<RTYPE> newData, Rcpp::CharacterVector newRowNames)
{
	std::vector<std::string> localColNames = extractElementOrColumnNames(newData);
	// ??? something with rownames? rcppExtractRowNames(table, newData, true);

	int equalizedColumnsLength = table->equalizeColumnsLengths();
	int previouslyAddedUnnamedCols = 0;

	for(int row=0; row<newRowNames.size(); row++)
		table->_rowNames[row + equalizedColumnsLength] = newRowNames[row];

	auto jsonMatrix = RcppMatrix_to_Vector2Json<RTYPE>(newData, table->getEscapeHtml());

	for(int col=0; col<jsonMatrix.size(); col++)
		previouslyAddedUnnamedCols = table->pushbackToColumnInData(std::vector<Json::Value>({jsonMatrix[col]}), localColNames.size() > col ? localColNames[col] : "", equalizedColumnsLength, previouslyAddedUnnamedCols);
}

void rcppTableSetData(jaspTable * table, Rcpp::RObject newData)
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	jaspPrint("jaspTable::setData");
#endif
	if(newData.isNULL())
	{
		table->_data.clear();
		return;
	}

	//Maybe this is overkill?
	if(Rcpp::is<Rcpp::DataFrame>(newData))				rcppSetDataFromList(table, convertFactorsToCharacters((Rcpp::DataFrame)	newData));
	else if(Rcpp::is<Rcpp::List>(newData))				rcppSetDataFromList(table, (Rcpp::List)									newData);

	else if(Rcpp::is<Rcpp::NumericMatrix>(newData))		rcppSetDataFromMatrix<REALSXP>(table, (Rcpp::NumericMatrix)	newData);
	else if(Rcpp::is<Rcpp::LogicalMatrix>(newData))		rcppSetDataFromMatrix<LGLSXP>(table, (Rcpp::LogicalMatrix)		newData);
	else if(Rcpp::is<Rcpp::IntegerMatrix>(newData))		rcppSetDataFromMatrix<INTSXP>(table, (Rcpp::IntegerMatrix)		newData);
	else if(Rcpp::is<Rcpp::StringMatrix>(newData))		rcppSetDataFromMatrix<STRSXP>(table, (Rcpp::StringMatrix)		newData);
	else if(Rcpp::is<Rcpp::CharacterMatrix>(newData))	rcppSetDataFromMatrix<STRSXP>(table, (Rcpp::CharacterMatrix)	newData);

	else if(Rcpp::is<Rcpp::NumericVector>(newData))		rcppSetDataFromVector<REALSXP>(table, (Rcpp::NumericVector)	newData);
	else if(Rcpp::is<Rcpp::LogicalVector>(newData))		rcppSetDataFromVector<LGLSXP>(table, (Rcpp::LogicalVector)		newData);
	else if(Rcpp::is<Rcpp::IntegerVector>(newData))		rcppSetDataFromVector<INTSXP>(table, (Rcpp::IntegerVector)	newData);
	else if(Rcpp::is<Rcpp::StringVector>(newData))		rcppSetDataFromVector<STRSXP>(table, (Rcpp::StringVector)	newData);
	else if(Rcpp::is<Rcpp::CharacterVector>(newData))	rcppSetDataFromVector<STRSXP>(table, (Rcpp::CharacterVector)	newData);

	else
		Rf_error("Cannot set this kind of data to a jaspTable, it is not understood. Try a list, dataframe, vector or matrix instead.");

	table->notifyParentOfChanges();
}

void rcppTableSetColumn(jaspTable * table, std::string columnName, Rcpp::RObject column)
{
	int colIndex = table->getDesiredColumnIndexFromNameForColumnAdding(columnName);

	if(Rcpp::is<Rcpp::NumericVector>(column))			rcppSetColumnFromVector<REALSXP>(table, (Rcpp::NumericVector)	column, colIndex);
	else if(Rcpp::is<Rcpp::LogicalVector>(column))		rcppSetColumnFromVector<LGLSXP>(table, (Rcpp::LogicalVector)	column, colIndex);
	else if(Rcpp::is<Rcpp::IntegerVector>(column))		rcppSetColumnFromVector<INTSXP>(table, (Rcpp::IntegerVector)	column, colIndex);
	else if(Rcpp::is<Rcpp::StringVector>(column))		rcppSetColumnFromVector<STRSXP>(table, (Rcpp::StringVector)	column, colIndex);
	else if(Rcpp::is<Rcpp::CharacterVector>(column))	rcppSetColumnFromVector<STRSXP>(table, (Rcpp::CharacterVector)	column, colIndex);
	else if(isMixedRObject(column))						rcppSetColumnFromMixedVector(table, (Rcpp::List)				column, colIndex);
	else if(Rcpp::is<Rcpp::List>(column))				rcppSetColumnFromList(table, (Rcpp::List)						column,	colIndex);
	else Rf_error("Did not get a vector or list as column..");

	table->notifyParentOfChanges();
}

void rcppTableAddColumns(jaspTable * table, Rcpp::RObject newData)
{
	if(newData.isNULL())
		return;

	//Maybe this is overkill?
	if(Rcpp::is<Rcpp::DataFrame>(newData))				rcppAddColumnsFromList(table, convertFactorsToCharacters((Rcpp::DataFrame)	newData));
	else if(Rcpp::is<Rcpp::List>(newData))				rcppAddColumnsFromList(table, (Rcpp::List)									newData);

	else if(Rcpp::is<Rcpp::NumericMatrix>(newData))		rcppAddColumnsFromMatrix<REALSXP>(table, (Rcpp::NumericMatrix)	newData);
	else if(Rcpp::is<Rcpp::LogicalMatrix>(newData))		rcppAddColumnsFromMatrix<LGLSXP>(table, (Rcpp::LogicalMatrix)	newData);
	else if(Rcpp::is<Rcpp::IntegerMatrix>(newData))		rcppAddColumnsFromMatrix<INTSXP>(table, (Rcpp::IntegerMatrix)	newData);
	else if(Rcpp::is<Rcpp::StringMatrix>(newData))		rcppAddColumnsFromMatrix<STRSXP>(table, (Rcpp::StringMatrix)	newData);
	else if(Rcpp::is<Rcpp::CharacterMatrix>(newData))	rcppAddColumnsFromMatrix<STRSXP>(table, (Rcpp::CharacterMatrix)newData);

	else if(Rcpp::is<Rcpp::NumericVector>(newData))		rcppAddColumnFromVector<REALSXP>(table, (Rcpp::NumericVector)	newData);
	else if(Rcpp::is<Rcpp::LogicalVector>(newData))		rcppAddColumnFromVector<LGLSXP>(table, (Rcpp::LogicalVector)	newData);
	else if(Rcpp::is<Rcpp::IntegerVector>(newData))		rcppAddColumnFromVector<INTSXP>(table, (Rcpp::IntegerVector)	newData);
	else if(Rcpp::is<Rcpp::StringVector>(newData))		rcppAddColumnFromVector<STRSXP>(table, (Rcpp::StringVector)	newData);
	else if(Rcpp::is<Rcpp::CharacterVector>(newData))	rcppAddColumnFromVector<STRSXP>(table, (Rcpp::CharacterVector)	newData);

	else
		Rf_error("Cannot add this kind of data as a column to a jaspTable, it is not understood. Try a list, dataframe, vector or matrix instead.");

	table->notifyParentOfChanges();
}

void rcppTableAddRows(jaspTable * table, Rcpp::RObject newData, Rcpp::CharacterVector rowNames)
{
	if(newData.isNULL())
		return;

	//Maybe this is overkill?
	if(Rcpp::is<Rcpp::DataFrame>(newData))				rcppAddRowsFromDataFrame(table, (Rcpp::DataFrame)				newData);
	else if(Rcpp::is<Rcpp::List>(newData))				rcppAddRowsFromList(table, (Rcpp::List)						newData, rowNames);

	else if(Rcpp::is<Rcpp::NumericMatrix>(newData))		rcppAddRowsFromMatrix<REALSXP>(table, (Rcpp::NumericMatrix)	newData, rowNames);
	else if(Rcpp::is<Rcpp::LogicalMatrix>(newData))		rcppAddRowsFromMatrix<LGLSXP>(table, (Rcpp::LogicalMatrix)		newData, rowNames);
	else if(Rcpp::is<Rcpp::IntegerMatrix>(newData))		rcppAddRowsFromMatrix<INTSXP>(table, (Rcpp::IntegerMatrix)		newData, rowNames);
	else if(Rcpp::is<Rcpp::StringMatrix>(newData))		rcppAddRowsFromMatrix<STRSXP>(table, (Rcpp::StringMatrix)		newData, rowNames);
	else if(Rcpp::is<Rcpp::CharacterMatrix>(newData))	rcppAddRowsFromMatrix<STRSXP>(table, (Rcpp::CharacterMatrix)	newData, rowNames);

	else
		Rf_error("Cannot add this kind of data as rows to a jaspTable, it is not understood. Try a list, dataframe or matrix instead.");

	table->notifyParentOfChanges();
}

void rcppTableAddRow(jaspTable * table, Rcpp::RObject newData, Rcpp::CharacterVector rowName)
{
	if(newData.isNULL())
		return;

	if		(Rcpp::is<Rcpp::List>(newData))				rcppAddRowFromList(table, (Rcpp::List)							newData, rowName);

	else if	(Rcpp::is<Rcpp::NumericVector>(newData))	rcppAddRowFromVector<REALSXP>(table, (Rcpp::NumericVector)		newData, rowName);
	else if	(Rcpp::is<Rcpp::LogicalVector>(newData))	rcppAddRowFromVector<LGLSXP>(table, (Rcpp::LogicalVector)		newData, rowName);
	else if	(Rcpp::is<Rcpp::IntegerVector>(newData))	rcppAddRowFromVector<INTSXP>(table, (Rcpp::IntegerVector)		newData, rowName);
	else if	(Rcpp::is<Rcpp::StringVector>(newData))		rcppAddRowFromVector<STRSXP>(table, (Rcpp::StringVector)		newData, rowName);
	else if	(Rcpp::is<Rcpp::CharacterVector>(newData))	rcppAddRowFromVector<STRSXP>(table, (Rcpp::CharacterVector)	newData, rowName);

	else
		Rf_error("Cannot add this kind of data as a row to a jaspTable, it is not understood. Try a list or vector instead.");

	table->notifyParentOfChanges();
}

void rcppTableAddColumnInfo(jaspTable * table, Rcpp::RObject name, Rcpp::RObject title, Rcpp::RObject type, Rcpp::RObject format, Rcpp::RObject combine, Rcpp::RObject overtitle)
{
	std::string colName = name.isNULL() ? table->defaultColName(table->_colNames.rowCount()) : Rcpp::as<std::string>(name);
	table->_specifiedColumns.insert(colName);

	table->_colNames.add(colName);

	std::string lastAddedColName = table->getColName(table->_colNames.rowCount() - 1);

	if(!title.isNULL())		table->_colTitles[		lastAddedColName ] = Rcpp::String(title);
	if(!type.isNULL())		table->_colTypes[		lastAddedColName ] = Rcpp::String(type);
	if(!format.isNULL())	table->_colFormats[	lastAddedColName ] = Rcpp::String(format);
	if(!overtitle.isNULL())	table->_colOvertitles[	lastAddedColName ] = Rcpp::String(overtitle);
	if(!combine.isNULL())	table->_colCombines[	lastAddedColName ] = Rcpp::as<bool>(combine);
}

void rcppTableAddFootnote(jaspTable * table, Rcpp::RObject message, Rcpp::RObject symbol, Rcpp::RObject col_names, Rcpp::RObject row_names)
{
	if (message.isNULL())
		Rf_error("One would expect a footnote to at least contain a message..");

	std::string strMessage	= Rcpp::String(message);
	std::string strSymbol	= symbol.isNULL() ? "" : Rcpp::String(symbol);

	std::vector<Json::Value> colNames;
	if (!col_names.isNULL())
		colNames = RcppVector_to_VectorJson(col_names, table->getEscapeHtml(), false);

	std::vector<Json::Value> rowNames;
	if (!row_names.isNULL())
		rowNames = RcppVector_to_VectorJson(row_names, table->getEscapeHtml(), false);

	table->addFootnote(strMessage, strSymbol, colNames, rowNames);
}

Rcpp::List rcppTableToRObject(jaspTable * table)
{
	Rcpp::DataFrame df;

	for (size_t col = 0; col < table->_data.size(); col++)
	{

		jaspTableColumnType type = table->deriveColumnType(col);

		switch(type)
		{

		// this could be a templated or overloaded function?
		case jaspTableColumnType::integer:
		{
			Rcpp::IntegerVector values(table->_data[col].size());
			for (size_t row = 0; row < table->_data[col].size(); row++)
			{
				const Json::Value & cell = table->_data[col][row];
				if (cell.isNumeric())
					values[row] = cell.asInt();
				else
					values[row] = NA_INTEGER; // placeholder/null -> NA
			}

			df[table->getColName(col)] = values;
			break;
		}
		case jaspTableColumnType::number:
		{
			Rcpp::NumericVector values(table->_data[col].size());
			for (size_t row = 0; row < table->_data[col].size(); row++)
			{
				const Json::Value & cell = table->_data[col][row];
				if (cell.isNumeric())
					values[row] = cell.asDouble();
				else
					values[row] = NA_REAL; // placeholder/null -> NA
			}

			df[table->getColName(col)] = values;
			break;
		}
		case jaspTableColumnType::logical:
		{
			Rcpp::LogicalVector values(table->_data[col].size());
			for (size_t row = 0; row < table->_data[col].size(); row++)
				values[row] = table->_data[col][row].asBool();

			df[table->getColName(col)] = values;

			break;
		}
		case jaspTableColumnType::string:
		case jaspTableColumnType::various:
		case jaspTableColumnType::unknown:
		case jaspTableColumnType::composite:
		{
			Rcpp::StringVector values(table->_data[col].size());
			for (size_t row = 0; row < table->_data[col].size(); row++)
				values[row] = decodeColumnNames(table->_data[col][row].asString());

			df[decodeColumnNames(table->getColName(col))] = values;
			break;
		}
		case jaspTableColumnType::mixed:
		{

			Rcpp::List valuesData(table->_data[col].size());
			Rcpp::StringVector valuesTypes(table->_data[col].size());
			Rcpp::List valuesFormats(table->_data[col].size());
			for (size_t row = 0; row < table->_data[col].size(); row++)
			{
				valuesTypes[row] = table->_data[col][row]["type"].asString();

				if		(valuesTypes[row] == "number")	valuesData[row] = table->_data[col][row]["value"].asDouble();
				else if (valuesTypes[row] == "pvalue")	valuesData[row] = table->_data[col][row]["value"].asDouble();
				else if (valuesTypes[row] == "integer")	valuesData[row] = table->_data[col][row]["value"].asInt();
				else if (valuesTypes[row] == "string")	valuesData[row] = decodeColumnNames(table->_data[col][row]["value"].asString());

				if (!table->_data[col][row]["format"].isNull())
					valuesFormats[row] = table->_data[col][row]["format"].asString();
			}

			Rcpp::Environment jaspBase = Rcpp::Environment::namespace_env("jaspBase");
			Rcpp::Function createMixedColumn = jaspBase["createMixedColumn"];
			Rcpp::List values = createMixedColumn(valuesData, valuesTypes, valuesFormats);
			df[decodeColumnNames(table->getColName(col))] = values;
			break;
		}
		// this case is probably unnecessary
		case jaspTableColumnType::null:
		{
			df[table->getColName(col)] = R_NilValue;
			break;
		}

		}
	}

	// footnotes toRObject (not very efficient, verbatim from the old footnotes::toRObject)
	Rcpp::List	notes;

	for (const auto & textRest : table->_footnotes._data)
		for(const auto & symbolRest : textRest.second)
			for(const footnotesNamespace::tableFields & fields : symbolRest.second)
			{
				Rcpp::List note = Rcpp::List::create(
					Rcpp::Named("text")		= textRest.first,
					Rcpp::Named("symbol")	= symbolRest.first
//					TODO: I do not understand the data in here, or how to convert it to R...
//					Rcpp::Named("rows")		= fields.rowsToJSON(),
//					Rcpp::Named("cols")		= fields.colsToJSON()
				);
				notes.push_back(note);
			}

	df.attr("footnotes")  = notes;
	df.attr("title") = decodeColumnNames(table->_title);
	df.attr("class") = Rcpp::CharacterVector({"jaspTableWrapper", "jaspWrapper", "data.frame"});

	std::vector<std::string> rowNames;
	const size_t rowCount = table->_data.empty() ? 0 : table->_data[0].size(); // empty table (e.g. no variables selected) has no rows
	rowNames.reserve(rowCount);
	for (size_t i = 0; i < rowCount; i++)
		rowNames.push_back(table->_rowNames[i] != "" ? decodeColumnNames(table->_rowNames[i]) : std::to_string(i + 1)); // R numbers from 1 to n by default

	df.attr("row.names") = rowNames;

	// the reason this function is not const
	Rcpp::Environment jaspObjectEnvironment = Rcpp::new_env();
	jaspObjectEnvironment.assign("jaspObject", Rcpp::as<Rcpp::RObject>(Rcpp::wrap(jaspTable_Interface(table))));
	df.attr("jaspObjectEnvironment") = jaspObjectEnvironment;

	return df;
}
