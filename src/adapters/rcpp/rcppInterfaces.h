#pragma once

// R-facing *_Interface wrappers for classes that already moved to src/core/.
// This header grows as more classes move (commits 05-08 of the phase-1 plan).

#include <Rcpp.h>
#include "jaspObjectInterface.h"
#include "rcppConversions.h"
#include "jaspHtml.h"
#include "jaspQmlSource.h"

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
