#pragma once

// R-facing *_Interface wrappers for classes that already moved to src/core/.
// This header grows as more classes move (commits 05-08 of the phase-1 plan).

#include <Rcpp.h>
#include "jaspObjectInterface.h"
#include "rcppConversions.h"
#include "rcppPlot.h"
#include "jaspHtml.h"
#include "jaspQmlSource.h"
#include "jaspReport.h"
#include "jaspState.h"
#include "jaspPlot.h"

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
