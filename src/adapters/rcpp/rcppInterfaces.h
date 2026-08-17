#pragma once

// R-facing *_Interface wrappers for classes that already moved to src/core/.
// This header grows as more classes move (commits 05-08 of the phase-1 plan).

#include <Rcpp.h>
#include "jaspObjectInterface.h"
#include "rcppConversions.h"
#include "jaspHtml.h"
#include "jaspQmlSource.h"
#include "jaspReport.h"
#include "jaspState.h"

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
