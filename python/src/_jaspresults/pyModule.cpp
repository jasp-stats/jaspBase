// pybind11 bindings for the R-free jaspResults core (Phase 2 preview).
//
// This is the seed of the planned `python/` package. It exposes the core
// object tree and the Python-native table ingest defined in pyConversions.cpp
// (§3.2 of tmp/plan-python-interface.md). Ownership mirrors the R engine:
// objects are owned by jaspObject::allocatedObjects and released via
// destroyAllAllocatedObjects(), so Python holders use py::nodelete.

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/functional.h>

#include <string>
#include <vector>

#include "jaspResults.h"
#include "jaspContainer.h"
#include "jaspHtml.h"
#include "jaspReport.h"
#include "jaspQmlSource.h"
#include "jaspTable.h"
#include "jaspPlot.h"
#include "jaspState.h"
#include "jaspColumn.h"
#include "jaspHost.h"
#include "pyConversions.h"

namespace py = pybind11;

// Keep a Python callable alive across C++ invocations. Deliberately leaked:
// destroying a static py::function after interpreter shutdown deadlocks.
static py::function * g_sendFunc = nullptr;
static void sendFuncTrampoline(const char * json)
{
	if(g_sendFunc)
		(*g_sendFunc)(std::string(json ? json : ""));
}

static py::function * g_logFunc = nullptr;
static void logFuncTrampoline(const std::string & msg)
{
	if(g_logFunc)
		(*g_logFunc)(msg);
	else
		fprintf(stdout, "%s", msg.c_str());
}

PYBIND11_MODULE(_jaspresults, m)
{
	m.doc() = "Python bindings for the R-free jaspResults core";

	// Explicit "NaN" cell marker (R distinguishes NA and NaN; Python cannot).
	py::class_<pyNaNString>(m, "NaNString")
		.def(py::init<>());
	m.attr("NaNString") = py::cast(pyNaNString());

	// ---- jaspObject ----
	py::class_<jaspObject, std::unique_ptr<jaspObject, py::nodelete>>(m, "jaspObject")
		.def_property("title", [](jaspObject & o){ return o._title; },
								[](jaspObject & o, std::string t){ o._title = t; })
		.def_property("info", [](jaspObject & o){ return o._info; },
								[](jaspObject & o, std::string i){ o._info = i; })
		.def_property("position", [](jaspObject & o){ return o._position; },
									[](jaspObject & o, int p){ o._position = p; })
		.def("type", &jaspObject::type)
		.def("addMessage", [](jaspObject & o, std::string msg){ o.addMessage(msg); })
		.def("addCitation", [](jaspObject & o, std::string c){ o.addCitation(c); })
		.def("setError", [](jaspObject & o, std::string msg){ o.setError(msg); })
		.def("getError", &jaspObject::getError)
		.def("dependOnOptions", &jaspObject::dependOnOptions)
		.def("setOptionMustBeDependency", [](jaspObject & o, std::string name, py::object val)
		{
			o.setOptionMustBeDependency(name, pyCellOrMixedToJsonValue(val, o.getEscapeHtml()));
		})
		.def("toHtml", &jaspObject::toHtml)
	;

	// ---- jaspHtml ----
	py::class_<jaspHtml, jaspObject, std::unique_ptr<jaspHtml, py::nodelete>>(m, "jaspHtml")
		.def(py::init<std::string>(), py::arg("text") = "")
		.def_property("text", &jaspHtml::getText, &jaspHtml::setText)
		.def_property("elementType", [](jaspHtml & h){ return h._elementType; },
									 [](jaspHtml & h, std::string t){ h._elementType = t; })
	;

	// ---- jaspReport ----
	py::class_<jaspReport, jaspObject, std::unique_ptr<jaspReport, py::nodelete>>(m, "jaspReport")
		.def(py::init<std::string, bool>(), py::arg("text") = "", py::arg("report") = false)
		.def_property("text", &jaspReport::getText, &jaspReport::setText)
		.def_property_readonly("isReport", [](jaspReport & r){ return r._report; })
	;

	// ---- jaspQmlSource ----
	py::class_<jaspQmlSource, jaspObject, std::unique_ptr<jaspQmlSource, py::nodelete>>(m, "jaspQmlSource")
		.def(py::init<std::string>(), py::arg("sourceID") = "")
		.def("setValue", [](jaspQmlSource & q, py::object val){ q.setValue(pyCellOrMixedToJsonValue(val, q.getEscapeHtml())); })
		.def("getValue", &jaspQmlSource::getValue)
	;

	// ---- jaspTable ----
	py::class_<jaspTable, jaspObject, std::unique_ptr<jaspTable, py::nodelete>>(m, "jaspTable")
		.def(py::init<std::string>(), py::arg("title") = "")
		.def("setData", &pyIngestSetData,
			 py::arg("data"), py::arg("col_names") = py::none(), py::arg("row_names") = py::none())
		.def("setColumn", &pyIngestSetColumn, py::arg("name"), py::arg("column"))
		.def("addColumns", &pyIngestAddColumns, py::arg("data"))
		.def("addRows", &pyIngestAddRows, py::arg("data"), py::arg("row_names") = std::vector<std::string>())
		.def("addRow", &pyIngestAddRow, py::arg("row"), py::arg("row_name") = "")
		.def("addColumnInfo", &pyIngestAddColumnInfo,
			 py::arg("name") = py::none(), py::arg("title") = py::none(), py::arg("type") = py::none(),
			 py::arg("format") = py::none(), py::arg("combine") = py::none(), py::arg("overtitle") = py::none())
		.def("addFootnote", &pyIngestAddFootnote,
			 py::arg("message"), py::arg("symbol") = py::none(),
			 py::arg("col_names") = py::none(), py::arg("row_names") = py::none())
		.def("setColNames", [](jaspTable & t, py::object v){ auto rf = pyToStringRowsAndFields(v); t.setColNames(rf.first, rf.second); })
		.def("setColTypes", [](jaspTable & t, py::object v){ auto rf = pyToStringRowsAndFields(v); t.setColTypes(rf.first, rf.second); })
		.def("setColTitles", [](jaspTable & t, py::object v){ auto rf = pyToStringRowsAndFields(v); t.setColTitles(rf.first, rf.second); })
		.def("setColOvertitles", [](jaspTable & t, py::object v){ auto rf = pyToStringRowsAndFields(v); t.setColOvertitles(rf.first, rf.second); })
		.def("setColFormats", [](jaspTable & t, py::object v){ auto rf = pyToStringRowsAndFields(v); t.setColFormats(rf.first, rf.second); })
		.def("setColCombines", [](jaspTable & t, py::object v){ auto rf = pyToBoolRowsAndFields(v); t.setColCombines(rf.first, rf.second); })
		.def("setRowNames", [](jaspTable & t, py::object v){ auto rf = pyToStringRowsAndFields(v); t.setRowNames(rf.first, rf.second); })
		.def("setRowTitles", [](jaspTable & t, py::object v){ auto rf = pyToStringRowsAndFields(v); t.setRowTitles(rf.first, rf.second); })
		.def("setExpectedSize", &jaspTable::setExpectedSize)
		.def("setExpectedRows", &jaspTable::setExpectedRows)
		.def("setExpectedColumns", &jaspTable::setExpectedColumns)
		.def_property("status", [](jaspTable & t){ return t._status; },
								[](jaspTable & t, std::string s){ t._status = s; })
		.def_property("transpose", [](jaspTable & t){ return t._transposeTable; },
								   [](jaspTable & t, bool v){ t._transposeTable = v; })
		.def_property("transposeWithOvertitle", [](jaspTable & t){ return t._transposeWithOvertitle; },
												[](jaspTable & t, bool v){ t._transposeWithOvertitle = v; })
		.def_property("showSpecifiedColumnsOnly", [](jaspTable & t){ return t._showSpecifiedColumnsOnly; },
												  [](jaspTable & t, bool v){ t._showSpecifiedColumnsOnly = v; })
		.def("complete", &jaspTable::complete)
		// Test/introspection helper: dump the raw column-major cells as JSON.
		.def("_debugCells", [](jaspTable & t)
		{
			Json::Value arr(Json::arrayValue);
			for(auto & col : t._data)
			{
				Json::Value c(Json::arrayValue);
				for(auto & cell : col) c.append(cell);
				arr.append(c);
			}
			return arr.toStyledString();
		})
	;

	// ---- jaspPlot ----
	py::class_<jaspPlot, jaspObject, std::unique_ptr<jaspPlot, py::nodelete>>(m, "jaspPlot")
		.def(py::init<std::string>(), py::arg("title") = "")
		.def_property("status", [](jaspPlot & p){ return p._status; },
								[](jaspPlot & p, std::string s){ p._status = s; })
		.def_readwrite("filePathPng", &jaspPlot::_filePathPng)
		.def_readwrite("width", &jaspPlot::_width)
		.def_readwrite("height", &jaspPlot::_height)
		.def_readwrite("aspectRatio", &jaspPlot::_aspectRatio)
	;

	// ---- jaspState ----
	py::class_<jaspState, jaspObject, std::unique_ptr<jaspState, py::nodelete>>(m, "jaspState")
		.def(py::init<std::string>(), py::arg("title") = "")
		.def("setObject", [](jaspState & s, py::object o){ s.setObject(std::any(pyCellOrMixedToJsonValue(o, s.getEscapeHtml()))); })
		.def("getObjectJson", [](jaspState & s)
		{
			std::any a = s.getObject();
			if(auto * v = std::any_cast<Json::Value>(&a))
				return v->toStyledString();
			return std::string("");
		})
	;

	// ---- jaspColumn ----
	py::class_<jaspColumn, jaspObject, std::unique_ptr<jaspColumn, py::nodelete>>(m, "jaspColumn")
		.def(py::init<std::string, bool>(), py::arg("columnName") = "", py::arg("computed") = false)
	;

	// ---- jaspContainer ----
	py::class_<jaspContainer, jaspObject, std::unique_ptr<jaspContainer, py::nodelete>> containerCls(m, "jaspContainer");
	containerCls
		.def(py::init<std::string>(), py::arg("title") = "")
		.def_property_readonly("length", &jaspContainer::length)
		.def("insert", [](jaspContainer & c, std::string field, jaspObject * obj){ c.insert(field, obj); },
					   py::arg("field"), py::arg("obj"))
		.def("at", [](jaspContainer & c, std::string field) -> jaspObject * { return c.at(field); },
					 py::return_value_policy::reference)
		.def_property("initCollapsed", [](jaspContainer & c){ return c._initiallyCollapsed; },
									   [](jaspContainer & c, bool v){ c._initiallyCollapsed = v; })
	;

	// ---- jaspResults ----
	py::class_<jaspResults, jaspContainer, std::unique_ptr<jaspResults, py::nodelete>>(m, "jaspResults")
		.def(py::init<std::string>(), py::arg("title"))
		.def("getResults", &jaspResults::getResults)
		.def("setOptions", &jaspResults::setOptions)
		.def("changeOptions", &jaspResults::changeOptions)
		.def("setErrorMessage", &jaspResults::setErrorMessage)
		.def("send", [](jaspResults & r, std::string otherMsg){ r.send(otherMsg); }, py::arg("otherMsg") = "")
		.def("complete", &jaspResults::complete)
		.def("saveResults", &jaspResults::saveResults)
		.def("prepareForWriting", &jaspResults::prepareForWriting)
		.def("finishWriting", &jaspResults::finishWriting)
		.def_property("status", [](jaspResults & r){ return r.getStatus(); },
								[](jaspResults & r, std::string s){ r.setStatus(s); })
		.def_property("relativePathKeep", [](jaspResults & r){ return r._relativePathKeep; },
										  [](jaspResults & r, std::string v){ r._relativePathKeep = v; })
	;

	// ---- module-level functions ----
	m.def("setResponseData", &jaspResults::setResponseData);
	m.def("setSaveLocation", &jaspResults::setSaveLocation);
	m.def("setWriteSealLocation", &jaspResults::setWriteSealLocation);
	m.def("setBaseCitation", &jaspResults::setBaseCitation);
	m.def("setDeveloperMode", &jaspObject::setDeveloperMode);
	m.def("destroyAllAllocatedObjects", &jaspObject::destroyAllAllocatedObjects);

	m.def("setSendFunc", [](py::function f)
	{
		if(!g_sendFunc)
			g_sendFunc = new py::function(std::move(f));
		else
			*g_sendFunc = std::move(f);
		jaspResults::setSendFunc(&sendFuncTrampoline);
	});

	m.def("setLogFunc", [](py::function f)
	{
		if(!g_logFunc)
			g_logFunc = new py::function(std::move(f));
		else
			*g_logFunc = std::move(f);
		jaspHost::logString = &logFuncTrampoline;
	});
}
