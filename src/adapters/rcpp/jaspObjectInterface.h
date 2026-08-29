#pragma once

// R-facing interface wrapper of jaspObject plus the RCPP_MODULE helper macros.
// Moved verbatim from the old src/jaspObject.h (behaviour and signatures must
// stay bit-identical); conversions go through rcppConversions.h, toRObject()
// through rcppToRObject.h.

#include <Rcpp.h>
#include "jaspObject.h"
#include "rcppConversions.h"
#include "rcppToRObject.h"

#define JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(JASP_TYPE, PROP_TYPE, PROP_NAME, PROP_CAPITALIZED_NAME) \
	void set ## PROP_CAPITALIZED_NAME (PROP_TYPE new ## PROP_CAPITALIZED_NAME) { ((JASP_TYPE *)myJaspObject)->PROP_NAME = new ## PROP_CAPITALIZED_NAME; myJaspObject->notifyParentOfChanges(); } \
	PROP_TYPE get ## PROP_CAPITALIZED_NAME () { return ((JASP_TYPE *)myJaspObject)->PROP_NAME; }

#define JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NO_NOTIFY(JASP_TYPE, PROP_TYPE, PROP_NAME, PROP_CAPITALIZED_NAME) \
	void set ## PROP_CAPITALIZED_NAME (PROP_TYPE new ## PROP_CAPITALIZED_NAME) { ((JASP_TYPE *)myJaspObject)->PROP_NAME = new ## PROP_CAPITALIZED_NAME; } \
	PROP_TYPE get ## PROP_CAPITALIZED_NAME () { return ((JASP_TYPE *)myJaspObject)->PROP_NAME; }

#define JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NATIVE_STRING(JASP_TYPE, PROP_NAME, PROP_CAPITALIZED_NAME) \
void set ## PROP_CAPITALIZED_NAME (Rcpp::String new ## PROP_CAPITALIZED_NAME) { ((JASP_TYPE *)myJaspObject)->PROP_NAME =  new ## PROP_CAPITALIZED_NAME; myJaspObject->notifyParentOfChanges(); } \
Rcpp::String get ## PROP_CAPITALIZED_NAME () { return ((JASP_TYPE *)myJaspObject)->PROP_NAME; }

class jaspObject_Interface
{
public:
	jaspObject_Interface(jaspObject * dataObj) : myJaspObject(dataObj)
	{
#ifdef JASP_RESULTS_DEBUG_TRACES
		std::cout << "Interface to " << dataObj->objectTitleString() << " is created!\n"<<std::flush;
#endif
	}

	jaspObject_Interface(const jaspObject_Interface* copyMe)
	{
#ifdef JASP_RESULTS_DEBUG_TRACES
		std::cout << "Interface to " << copyMe->myJaspObject->objectTitleString() << " is copied!\n"<<std::flush;
#endif
		myJaspObject = copyMe->myJaspObject;
	}

	void		print()								{ myJaspObject->print(); }
	void		addMessage(Rcpp::String msg)		{ myJaspObject->addMessage(std::string(msg)); }
	std::string	toHtml()							{ return myJaspObject->toHtml(); }
	std::string	type()								{ return myJaspObject->type(); }
	void		printHtml()							{ jaspPrint(myJaspObject->toHtml()); }

	void		setOptionMustBeDependency(std::string optionName, Rcpp::RObject mustBeThis)								{ myJaspObject->setOptionMustBeDependency(optionName, RObject_to_JsonValue(mustBeThis, myJaspObject->getEscapeHtml()));					}
	void		setOptionMustContainDependency(std::string optionName, Rcpp::RObject mustContainThis)					{ myJaspObject->setOptionMustContainDependency(optionName, RObject_to_JsonValue(mustContainThis, myJaspObject->getEscapeHtml()));		}
	void		dependOnNestedOptions(Rcpp::CharacterVector optionName)													{ myJaspObject->dependOnNestedOptions(Rcpp::as<std::vector<std::string>>(optionName));													}
	void		setNestedOptionMustContainDependency(Rcpp::CharacterVector optionName, Rcpp::RObject mustContainThis)	{ myJaspObject->setNestedOptionMustContainDependency(Rcpp::as<std::vector<std::string>>(optionName), RObject_to_JsonValue(mustContainThis, myJaspObject->getEscapeHtml()));	}
	void		dependOnOptions(Rcpp::CharacterVector listOptions)														{ myJaspObject->dependOnOptions(Rcpp::as<std::vector<std::string>>(listOptions));														}
	void		copyDependenciesFromJaspObject(jaspObject_Interface * other)											{ myJaspObject->copyDependenciesFromJaspObject(other->myJaspObject);				}
	void		addCitation(Rcpp::String fullCitation)																	{ myJaspObject->addCitation(std::string(fullCitation));			}

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NATIVE_STRING(jaspObject, _title,		Title)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NATIVE_STRING(jaspObject, _info,		Info)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspObject, int,			_position,	Position)

	void		setError(Rcpp::String message)		{ myJaspObject->setError(std::string(message)); }
	bool		getError()							{ return myJaspObject->getError(); }

	Rcpp::List	toRObject()							{ return rcppToRObject(myJaspObject); }

	jaspObject * returnMyJaspObject() { return myJaspObject; }

protected:
		jaspObject * myJaspObject = NULL;
};

#define JASP_OBJECT_FINALIZER_LAMBDA(JASP_TYPE) //.finalizer( [](JASP_TYPE * obj) { std::cout << "finalizerLambda " #JASP_TYPE " Called\n" << std::flush;  jaspObjectFinalizer(obj); })

#define JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE) create_ ## JASP_TYPE
#define JASP_OBJECT_CREATOR_FUNCTIONNAME_STR(JASP_TYPE) "create_cpp_" #JASP_TYPE
#define JASP_OBJECT_CREATOR(JASP_TYPE) JASP_TYPE ## _Interface * JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE)(Rcpp::String title) { return new JASP_TYPE ## _Interface (new JASP_TYPE(title)); }
#define JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(JASP_TYPE) Rcpp::function(JASP_OBJECT_CREATOR_FUNCTIONNAME_STR(JASP_TYPE), &JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE))
#define JASP_OBJECT_CREATOR_ARG(JASP_TYPE, EXTRA_ARG) JASP_TYPE ## _Interface * JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE)(Rcpp::String title, Rcpp::RObject EXTRA_ARG) { return new JASP_TYPE ## _Interface (new JASP_TYPE(title, EXTRA_ARG)); }


RCPP_EXPOSED_CLASS_NODECL(jaspObject_Interface)
