#include "rcppToRObject.h"
#include "jaspObject.h"
#include "jaspContainer.h"
#include "jaspTable.h"
#include "jaspPlot.h"
#include "jaspHtml.h"

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
	case jaspObjectType::html:			return static_cast<jaspHtml*>(obj)->toRObject();
	default:							return R_NilValue; // old jaspObject::toRObject() default
	}
}
