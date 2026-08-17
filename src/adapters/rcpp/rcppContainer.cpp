// R-backed jaspContainer logic moved from the old src/jaspContainer.cpp.
// Behaviour is kept identical: insert dispatches on the exposed Rcpp wrapper
// classes, wrapJaspObject maps a core jaspObject* back to the right *_Interface
// SEXP, and toRObject builds the R wrapper list.

#include "rcppContainer.h"
#include "jaspContainer.h"
#include "jaspTable.h"
#include "jaspColumn.h"
#include "rcppInterfaces.h"
#include "rcppConversions.h"
#include "rcppToRObject.h"

void rcppContainerInsert(jaspContainer * container, std::string field, Rcpp::RObject value)
{
	if(value.isNULL())
	{
		container->insert(field, nullptr); //core insert erases the field when given nullptr

		return;
	}

	jaspObject * obj = nullptr;


	if(Rcpp::is<jaspObject_Interface>(value))			obj = Rcpp::as<jaspObject_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspContainer_Interface>(value))	obj = Rcpp::as<jaspContainer_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspQmlSource_Interface>(value))	obj = Rcpp::as<jaspQmlSource_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspColumn_Interface>(value))		obj = Rcpp::as<jaspColumn_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspReport_Interface>(value))		obj = Rcpp::as<jaspReport_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspTable_Interface>(value))		obj = Rcpp::as<jaspTable_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspState_Interface>(value))		obj = Rcpp::as<jaspState_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspPlot_Interface>(value))		obj = Rcpp::as<jaspPlot_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<jaspHtml_Interface>(value))		obj = Rcpp::as<jaspHtml_Interface>(value).returnMyJaspObject();
	else if(Rcpp::is<Rcpp::List>(value))				obj = (jaspObject*)(rcppContainerFromList(Rcpp::as<Rcpp::List>(value)));
	else												throw std::runtime_error("Unhandled Rcpp Object type");

	container->insert(field, obj);
}

jaspContainer * rcppContainerFromList(Rcpp::List convertThis)
{
	std::vector<std::string> colNamesVec = extractElementOrColumnNames(convertThis);

	if(convertThis.size() > colNamesVec.size())
		Rf_error("If you add a list() to jaspResults or a jaspContainer each element should be named!");

	jaspContainer * newContainer = new jaspContainer();

	for(int i=0; i<convertThis.size(); i++)
		if(colNamesVec[i] == "title")
			newContainer->_title = Rcpp::String(Rcpp::RObject(convertThis[i]));
		else
			rcppContainerInsert(newContainer, colNamesVec[i], convertThis[i]);

	return newContainer;
}

Rcpp::RObject rcppWrapJaspObject(jaspObject * ref)
{
	switch(ref->getType())
	{
	case jaspObjectType::container:	return Rcpp::wrap(jaspContainer_Interface(ref));
	case jaspObjectType::qmlSource:	return Rcpp::wrap(jaspQmlSource_Interface(ref));
	case jaspObjectType::column:	return Rcpp::wrap(jaspColumn_Interface(ref));
	case jaspObjectType::report:	return Rcpp::wrap(jaspReport_Interface(ref));
	case jaspObjectType::table:		return Rcpp::wrap(jaspTable_Interface(ref));
	case jaspObjectType::state:		return Rcpp::wrap(jaspState_Interface(ref));
	case jaspObjectType::html:		return Rcpp::wrap(jaspHtml_Interface(ref));
	case jaspObjectType::plot:		return Rcpp::wrap(jaspPlot_Interface(ref));
	default:						return R_NilValue;
	}
}

Rcpp::RObject rcppContainerAt(jaspContainer * container, std::string field)
{
	jaspObject * ref = container->at(field);
	if(ref == nullptr)
		return R_NilValue;

	return rcppWrapJaspObject(ref);
}

Rcpp::List rcppContainerToRObject(jaspContainer * container) /*const*/
{

	std::vector<std::string> keys = container->getSortedDataFields();
	Rcpp::List lst;

	for (const auto & key : keys)
	{

		jaspObject* child = container->getJaspObjectFromData(key);

		Rcpp::List Robj = rcppToRObject(child);
		if (Robj.length() > 0)
			lst.push_back(Robj, key);
	}

	lst.attr("class") = Rcpp::CharacterVector({"jaspContainerWrapper", "jaspWrapper"});
	lst.attr("title") = container->_title;

	// the reason this function is not const
	Rcpp::Environment jaspObjectEnvironment = Rcpp::new_env();
	jaspObjectEnvironment.assign("jaspObject", Rcpp::as<Rcpp::RObject>(Rcpp::wrap(jaspContainer_Interface(container))));
	lst.attr("jaspObjectEnvironment") = jaspObjectEnvironment;

	return lst;
}

Rcpp::RObject jaspContainer_Interface::findObjectWithUniqueNestedName(std::string uniqueNestedName)
{
	jaspObject * found = ((jaspContainer*)myJaspObject)->findObjectWithUniqueNestedName(uniqueNestedName);

	if(found == nullptr)
		return R_NilValue;

	return rcppWrapJaspObject(found);
}
