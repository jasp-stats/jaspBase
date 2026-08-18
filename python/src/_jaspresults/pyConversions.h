#pragma once

// Python -> jaspTable conversions: the §3.1 cell contract and §3.2 ingest
// dispatch from tmp/plan-python-interface.md. Mirrors the R adapters in
// src/adapters/rcpp (rcppConversions + rcppTableIngest) behaviour-for-behaviour
// so that the same table built from R and from Python produces byte-identical
// JSON. Documented deviations (Python can't express R's NA/NaN split etc.)
// live in the plan's §3.1/§3.2 tables.

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <json/json.h>
#include <string>
#include <vector>
#include "jaspTable.h"

namespace py = pybind11;

///Marker object exported as jaspresults.NaNString: the only way to get R's
///`NaN` cell ("NaN") from Python, where NaN and NA are indistinguishable
///(Python's nan maps to R's NA -> "").
struct pyNaNString {};

///§3.1 scalar -> cell. Unknown types raise.
Json::Value					pyCellToJsonValue(const py::handle & h, bool escapeHtml);

///Like pyCellToJsonValue, but mixed-cell dicts become {value,type,format}
///objects and generic dicts/lists become JSON objects/arrays (mirrors
///RObject_to_JsonValue on R lists).
Json::Value					pyCellOrMixedToJsonValue(const py::handle & h, bool escapeHtml);

///mixed-cell parts: None -> JSON null (like R NULL), scalars like pyCellToJsonValue.
Json::Value					pyMixedPartToJsonValue(const py::handle & h, bool escapeHtml);

///Any Python sequence (list/tuple/ndarray/Series/Categorical/range/generator)
///-> column of cells.
std::vector<Json::Value>	pySequenceToCells(const py::handle & h, bool escapeHtml);

///Names helper: materialises the sequence and stringifies each element.
std::vector<std::string>	pyToStringVector(const py::handle & h);

///§3.2 ingest paths (dispatch documented in the plan). esc comes from the
///table (jaspObject::getEscapeHtml).
void pyIngestSetData(		jaspTable * table, py::object data, py::object colNames, py::object rowNames);
void pyIngestSetColumn(		jaspTable * table, std::string columnName, py::object column);
void pyIngestAddColumns(	jaspTable * table, py::object data);
void pyIngestAddRows(		jaspTable * table, py::object data, std::vector<std::string> rowNames);
void pyIngestAddRow(		jaspTable * table, py::object row, std::string rowName);
void pyIngestAddColumnInfo(	jaspTable * table, py::object name, py::object title, py::object type, py::object format, py::object combine, py::object overtitle);
void pyIngestAddFootnote(	jaspTable * table, std::string message, py::object symbol, py::object colNames, py::object rowNames);

///setColNames/setColTypes/... accept a list (positional) or a dict
///(positional + fieldnames), mirroring the R named-list -> rows+fields split.
std::pair<std::vector<std::string>, std::map<std::string, std::string>>	pyToStringRowsAndFields(const py::handle & h);
std::pair<std::vector<bool>, std::map<std::string, bool>>					pyToBoolRowsAndFields(const py::handle & h);

///Type checks (numpy/pandas optional; false when the module is missing).
bool pyIsDataFrame(const py::handle & h);
bool pyIsSeries(const py::handle & h);
bool pyIsNdArray(const py::handle & h);
bool pyIsCategorical(const py::handle & h);
