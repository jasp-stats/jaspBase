#pragma once

// R-backed jaspTable ingest & export, moved verbatim from the old
// src/jaspTable.{h,cpp}: the SEXP dispatch (data.frame/list/matrix/vector) for
// setData/setColumn/addColumns/addRows/addRow, the NULL-based addColumnInfo /
// addFootnote, and toRObject() incl. mixed columns. Core jaspTable keeps the
// neutral cell storage + JSON machinery (src/core/jaspTable.h).

#include <Rcpp.h>

class jaspTable;

void		rcppTableSetData(			jaspTable * table, Rcpp::RObject newData);
void		rcppTableSetColumn(			jaspTable * table, std::string columnName, Rcpp::RObject column);
void		rcppTableAddColumns(		jaspTable * table, Rcpp::RObject newColumns);
void		rcppTableAddRows(			jaspTable * table, Rcpp::RObject newRows,		Rcpp::CharacterVector rowNames);
void		rcppTableAddRow(			jaspTable * table, Rcpp::RObject newRow,			Rcpp::CharacterVector rowName);
void		rcppTableAddColumnInfo(		jaspTable * table, Rcpp::RObject name, Rcpp::RObject title, Rcpp::RObject type, Rcpp::RObject format, Rcpp::RObject combine, Rcpp::RObject overtitle);
void		rcppTableAddFootnote(		jaspTable * table, Rcpp::RObject message, Rcpp::RObject symbol, Rcpp::RObject col_names, Rcpp::RObject row_names);
Rcpp::List	rcppTableToRObject(			jaspTable * table);
