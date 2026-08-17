#include "rcppToRObject.h"
#include "jaspObject.h"
#include "jaspContainer.h"
#include "jaspTable.h"
#include "jaspPlot.h"
#include "jaspHtml.h"
#include "rcppInterfaces.h" // jaspHtml_Interface (and later interfaces)

static Rcpp::List jaspHtmlToRObject(jaspHtml * html)
{
	// mimics convertToJSON, could also be a named character vector since everything is a string
	Rcpp::List lst = Rcpp::List::create(
		Rcpp::Named("rawtext")		= html->_rawText,
		Rcpp::Named("text")			= html->convertTextToHtml(html->_rawText),
		Rcpp::Named("class")		= html->_class,
		Rcpp::Named("maxWidth")		= html->_maxWidth,
		Rcpp::Named("elementType")	= html->_elementType
	);

	lst.attr("title") = html->_title;
	lst.attr("class") = Rcpp::CharacterVector({"jaspHtmlWrapper", "jaspWrapper"});

	// the reason this function is not const
	Rcpp::Environment jaspObjectEnvironment = Rcpp::new_env();
	jaspObjectEnvironment.assign("jaspObject", Rcpp::as<Rcpp::RObject>(Rcpp::wrap(jaspHtml_Interface(html))));
	lst.attr("jaspObjectEnvironment") = jaspObjectEnvironment;

	return lst;
}

Rcpp::List rcppToRObject(jaspObject * obj)
{
	if(obj == nullptr)
		return R_NilValue;

	switch(obj->getType())
	{
	case jaspObjectType::container:
	case jaspObjectType::results:		return static_cast<jaspContainer*>(obj)->toRObject();
	case jaspObjectType::table:			return static_cast<jaspTable*>(obj)->toRObject();
	case jaspObjectType::plot:			return static_cast<jaspPlot*>(obj)->toRObject();
	case jaspObjectType::html:			return jaspHtmlToRObject(static_cast<jaspHtml*>(obj));
	default:							return R_NilValue; // old jaspObject::toRObject() default
	}
}
