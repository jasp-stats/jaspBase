#pragma once

// R-facing *_Interface wrappers for classes that already moved to src/core/.
// This header grows as more classes move (commits 05-08 of the phase-1 plan).

#include <Rcpp.h>
#include "jaspObjectInterface.h"
#include "rcppConversions.h"
#include "rcppPlot.h"
#include "rcppContainer.h"
#include "rcppTableIngest.h"
#include "jaspHtml.h"
#include "jaspQmlSource.h"
#include "jaspReport.h"
#include "jaspState.h"
#include "jaspPlot.h"
#include "jaspContainer.h"
#include "jaspList.h"
#include "jaspTable.h"

class jaspHtml_Interface : public jaspObject_Interface
{
public:
	jaspHtml_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void			setText(Rcpp::String newRawText) { 			static_cast<jaspHtml *>(myJaspObject)->setText(std::string(newRawText)); }
    Rcpp::String	getText() 						{ return 	static_cast<jaspHtml *>(myJaspObject)->getText(); }
    std::string		getHtml()						{ return	static_cast<jaspHtml *>(myJaspObject)->getHtml(); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspHtml, std::string,	_elementType,	ElementType)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspHtml, std::string,	_class,			Class)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspHtml, std::string,	_maxWidth,		MaxWidth)

};

RCPP_EXPOSED_CLASS_NODECL(jaspHtml_Interface)

class jaspQmlSource_Interface : public jaspObject_Interface
{
public:
	jaspQmlSource_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspQmlSource, std::string,	_sourceID,	SourceID)

	void			setValue(Rcpp::RObject obj)			{ jaspQmlSource * q = static_cast<jaspQmlSource*>(myJaspObject); q->setValue(RObject_to_JsonValue(obj, q->getEscapeHtml())); }
	std::string		getValue()							{ return static_cast<jaspQmlSource*>(myJaspObject)->getValue();	}
};

RCPP_EXPOSED_CLASS_NODECL(jaspQmlSource_Interface)

class jaspReport_Interface : public jaspObject_Interface
{
public:
	jaspReport_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

    void			setText(Rcpp::String newRawText) { 			static_cast<jaspReport *>(myJaspObject)->setText(std::string(newRawText)); }
    Rcpp::String	getText() 						{ return 	static_cast<jaspReport *>(myJaspObject)->getText(); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspReport, bool,	_report,	Report)
};

RCPP_EXPOSED_CLASS_NODECL(jaspReport_Interface)

class jaspState_Interface : public jaspObject_Interface
{
public:
	jaspState_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void setObject(Rcpp::RObject obj)	{			static_cast<jaspState*>(myJaspObject)->setObject(std::any(obj));	}
	Rcpp::RObject getObject()
	{
		std::any obj = static_cast<jaspState*>(myJaspObject)->getObject();
		if(!obj.has_value())
			return R_NilValue;
		try
		{
			return std::any_cast<Rcpp::RObject>(obj);
		}
		catch(const std::bad_any_cast &)
		{
			return R_NilValue;
		}
	}
};

RCPP_EXPOSED_CLASS_NODECL(jaspState_Interface)

class jaspPlot_Interface : public jaspObject_Interface
{
public:
	jaspPlot_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void setPlotObject(Rcpp::RObject plotObject)
	{
		jaspPlot * plot = static_cast<jaspPlot*>(myJaspObject);
		Rcpp::List plotInfo = Rcpp::List::create(
			Rcpp::_["obj"] = plotObject,
			Rcpp::_["width"] = plot->_width,
			Rcpp::_["height"] = plot->_height,
			Rcpp::_["revision"] = plot->_revision);
		plot->setPlotObject(std::any((Rcpp::RObject)plotInfo));
	}
	Rcpp::RObject getPlotObject() { return rcppGetPlotObject(static_cast<jaspPlot*>(myJaspObject)); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, std::string,	_filePathPng,	FilePathPng)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, std::string,	_status,		Status)

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, float,			_aspectRatio,	AspectRatio)

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, int,			_width,			Width)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, int,			_height,		Height)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, int,			_revision,		Revision)

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NO_NOTIFY(jaspPlot, bool,			_editing,				Editing)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NO_NOTIFY(jaspPlot, bool,			_resizedByUser,			ResizedByUser)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NO_NOTIFY(jaspPlot, std::string,	_interactiveJsonData,	InteractiveJsonData)

	///Set/export machine-readable data from R:
	///  plot$export <- list(medianDelta = 0.45, ciLow = 0.12, ciHigh = 0.78)
	///Appears in both the JSON results and the RDS (survives stripping).
	void		setExport(Rcpp::List exportData);
	Rcpp::List	getExport();
};

RCPP_EXPOSED_CLASS_NODECL(jaspPlot_Interface)

class jaspContainer_Interface : public jaspObject_Interface
{
public:
	jaspContainer_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	int length()																	{ return ((jaspContainer*)myJaspObject)->length(); }
	Rcpp::RObject	at(std::string field)											{ return rcppContainerAt((jaspContainer*)myJaspObject, field); }
	void			insert(std::string field, Rcpp::RObject value)					{ rcppContainerInsert((jaspContainer*)myJaspObject, field, value); }
	Rcpp::RObject	findObjectWithUniqueNestedName(std::string uniqueNestedName);

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspContainer, bool,	_initiallyCollapsed,	InitiallyCollapsed)
};

RCPP_EXPOSED_CLASS_NODECL(jaspContainer_Interface)

template<typename T>
class jaspList_Interface : public jaspObject_Interface
{
public:
	jaspList_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void insert(Rcpp::RObject field, T value)
	{
		if(Rcpp::is<Rcpp::NumericVector>(field) || Rcpp::is<Rcpp::IntegerVector>(field))
			static_cast<jaspList<T>*>(myJaspObject)->insertIndex(Rcpp::as<int>(field) - 1, value);
		else if(Rcpp::is<Rcpp::CharacterVector>(field) || Rcpp::is<Rcpp::StringVector>(field))
			static_cast<jaspList<T>*>(myJaspObject)->insertField(Rcpp::as<std::string>(field), value);
		else
			Rf_error("Did not get a number, integer or string to index on.");
	}

	T at(Rcpp::RObject field)
	{
		if(Rcpp::is<Rcpp::NumericVector>(field) || Rcpp::is<Rcpp::IntegerVector>(field))
			return static_cast<jaspList<T>*>(myJaspObject)->atIndex(Rcpp::as<int>(field) - 1);
		else if(Rcpp::is<Rcpp::CharacterVector>(field) || Rcpp::is<Rcpp::StringVector>(field))
			return static_cast<jaspList<T>*>(myJaspObject)->atField(Rcpp::as<std::string>(field));
		else
			Rf_error("Did not get a number, integer or string to index on.");

		return T();
	}

	void add(T value)	{ static_cast<jaspList<T>*>(myJaspObject)->add(value); }
};

typedef jaspList_Interface<std::string>	jaspStringlist_Interface;
typedef jaspList_Interface<double>		jaspDoublelist_Interface;
typedef jaspList_Interface<int>			jaspIntlist_Interface;
typedef jaspList_Interface<bool>		jaspBoollist_Interface;

RCPP_EXPOSED_CLASS_NODECL(jaspStringlist_Interface)
RCPP_EXPOSED_CLASS_NODECL(jaspDoublelist_Interface)
RCPP_EXPOSED_CLASS_NODECL(jaspIntlist_Interface)
RCPP_EXPOSED_CLASS_NODECL(jaspBoollist_Interface)

#define JASPLIST_MODULE_EXPORT(CLASS_NAME_CPP, CLASS_NAME_R)														\
Rcpp::class_<CLASS_NAME_CPP>(CLASS_NAME_R)																			\
	.derives<jaspObject_Interface>("jaspObject")																	\
	.method( "[[",		&CLASS_NAME_CPP::at,		"Access element by fieldname (string) or index (int) ")			\
	.method( "[[<-",	&CLASS_NAME_CPP::insert,	"Insert an element under index (int) or fieldname (string)")	\
	.method( "insert",	&CLASS_NAME_CPP::insert,	"Insert an element under index (int) or fieldname (string)")	\
	.method( "add",		&CLASS_NAME_CPP::add,		"Add an element at the end of the indexable list")				\
	JASP_OBJECT_FINALIZER_LAMBDA(CLASS_NAME_CPP)																	\
;

class jaspTable_Interface : public jaspObject_Interface
{
public:
	jaspTable_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	jaspStringlist_Interface	getColNames()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_colNames)		); }
	jaspStringlist_Interface	getColTypes()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_colTypes)		); }
	jaspStringlist_Interface	getColTitles()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_colTitles)		); }
	jaspStringlist_Interface	getColOvertitles()		{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_colOvertitles)	); }
	jaspStringlist_Interface	getColFormats()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_colFormats)		); }
	jaspBoollist_Interface		getColCombines()		{ return jaspBoollist_Interface(	&(((jaspTable*)myJaspObject)->_colCombines)		); }
	jaspStringlist_Interface	getRowNames()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_rowNames)		); }
	jaspStringlist_Interface	getRowTitles()			{ return jaspStringlist_Interface(	&(((jaspTable*)myJaspObject)->_rowTitles)		); }

	void setColNames(Rcpp::List newNames)
	{
		auto rf = rcppListToRowsAndFields<std::string>(newNames);
		((jaspTable*)myJaspObject)->setColNames(rf.first, rf.second);
	}
	void setColTypes(Rcpp::List newTypes)
	{
		auto rf = rcppListToRowsAndFields<std::string>(newTypes);
		((jaspTable*)myJaspObject)->setColTypes(rf.first, rf.second);
	}
	void setColTitles(Rcpp::List newTitles)
	{
		auto rf = rcppListToRowsAndFields<std::string>(newTitles);
		((jaspTable*)myJaspObject)->setColTitles(rf.first, rf.second);
	}
	void setColOvertitles(Rcpp::List newTitles)
	{
		auto rf = rcppListToRowsAndFields<std::string>(newTitles);
		((jaspTable*)myJaspObject)->setColOvertitles(rf.first, rf.second);
	}
	void setColFormats(Rcpp::List newFormats)
	{
		auto rf = rcppListToRowsAndFields<std::string>(newFormats);
		((jaspTable*)myJaspObject)->setColFormats(rf.first, rf.second);
	}
	void setColCombines(Rcpp::List newCombines)
	{
		auto rf = rcppListToRowsAndFields<bool>(newCombines);
		((jaspTable*)myJaspObject)->setColCombines(rf.first, rf.second);
	}
	void setRowNames(Rcpp::List newNames)
	{
		auto rf = rcppListToRowsAndFields<std::string>(newNames);
		((jaspTable*)myJaspObject)->setRowNames(rf.first, rf.second);
	}
	void setRowTitles(Rcpp::List newTitles)
	{
		auto rf = rcppListToRowsAndFields<std::string>(newTitles);
		((jaspTable*)myJaspObject)->setRowTitles(rf.first, rf.second);
	}

	void addColumnInfo(Rcpp::RObject name, Rcpp::RObject title, Rcpp::RObject type, Rcpp::RObject format, Rcpp::RObject combine, Rcpp::RObject overtitle)	{ rcppTableAddColumnInfo((jaspTable*)myJaspObject, name, title, type, format, combine, overtitle); }
	void addFootnote(Rcpp::RObject message, Rcpp::RObject symbol, Rcpp::RObject col_names, Rcpp::RObject row_names)											{ rcppTableAddFootnote((jaspTable*)myJaspObject, message, symbol, col_names, row_names); }

	void setData(Rcpp::RObject newData)							{ rcppTableSetData((jaspTable*)myJaspObject, newData);			}
	void addColumns(Rcpp::RObject newColumns)					{ rcppTableAddColumns((jaspTable*)myJaspObject, newColumns);	}

	void addRows(				Rcpp::RObject newRows,	Rcpp::CharacterVector rowNames)	{ rcppTableAddRows((jaspTable*)myJaspObject, newRows, rowNames);		}
	void addRowsWithoutNames(	Rcpp::RObject newRows)									{ rcppTableAddRows((jaspTable*)myJaspObject, newRows, Rcpp::CharacterVector());		}
	void addRow(				Rcpp::RObject newRow,	Rcpp::CharacterVector rowNames)	{ rcppTableAddRow((jaspTable*)myJaspObject, newRow, rowNames);			}
	void addRowWithoutNames(	Rcpp::RObject newRow)									{ rcppTableAddRow((jaspTable*)myJaspObject, newRow, Rcpp::CharacterVector());			}
	void setColumn(				std::string columnName, Rcpp::RObject column)			{ rcppTableSetColumn((jaspTable*)myJaspObject, columnName, column);	}

	void setExpectedSize(size_t columns, size_t rows)	{ ((jaspTable*)myJaspObject)->setExpectedSize(columns, rows);	}
	void setExpectedRows(size_t rows)					{ ((jaspTable*)myJaspObject)->setExpectedRows(rows);			}
	void setExpectedColumns(size_t columns)				{ ((jaspTable*)myJaspObject)->setExpectedColumns(columns);		}


	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspTable, bool,			_transposeTable,				TransposeTable)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspTable, bool,			_transposeWithOvertitle,		TransposeWithOvertitle)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspTable, std::string,	_status,						Status)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspTable, bool,			_showSpecifiedColumnsOnly,		ShowSpecifiedColumnsOnly)
};

RCPP_EXPOSED_CLASS_NODECL(jaspTable_Interface)
