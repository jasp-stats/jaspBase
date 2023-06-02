#ifndef JASPQMLSOURCE_H
#define JASPQMLSOURCE_H

#include "jaspObject.h"

class jaspQmlSource : public jaspObject
{
public:
					jaspQmlSource(const std::string & sourceID = "");

	void			setSourceID(const std::string & sourceID)							{ _sourceID = sourceID; }
	std::string		sourceID()										const				{ return _sourceID; }
	void			setValue(Rcpp::RObject Robj)										{ _json = RObject_to_JsonValue(Robj); _changed = true;	}
	std::string		getValue()										const				{ return _json.toStyledString();		}

	Json::Value		metaEntry()										const	override	{ return constructMetaEntry("qmlSource"); }
	Json::Value		dataEntry(std::string & errorMessage)			const	override;

	void			convertFromJSON_SetFields(Json::Value in)				override;
	Json::Value		convertToJSON()									const	override;

	std::string		dataToString(std::string prefix)				const	override	{ return jsonToPrefixedStrings(prefix + "\t"); }
	std::string		jsonToPrefixedStrings(std::string prefix = "")	const				{ return jsonToPrefixedStrings(_json, prefix); }
	std::string		jsonToPrefixedStrings(Json::Value val, std::string prefix) const;

	Json::Value		RcppVector_to_ArrayJson(Rcpp::RObject obj, bool throwError=true)	{ return VectorJson_to_ArrayJson(RcppVector_to_VectorJson(obj, throwError)); }

	bool			shouldBePartOfResultsJson(bool meta = false)	const	override;

	void			complete()	{ _complete = true; }
	bool			changed()										const				{ return _changed; }

	std::string		_sourceID;

protected:
	Json::Value		_json;
	bool			_complete	= false, ///<- This is used to keep the logfiles/resultjson small until the source is actually needed. Which is at complete only anyway
					_changed	= false;

};


class jaspQmlSource_Interface : public jaspObject_Interface
{
public:
	jaspQmlSource_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspQmlSource, std::string,	_sourceID,	SourceID)

	void			setValue(Rcpp::RObject obj)			{ ((jaspQmlSource*)myJaspObject)->setValue(obj);		}
	std::string		getValue()							{ return ((jaspQmlSource*)myJaspObject)->getValue();	}
};

RCPP_EXPOSED_CLASS_NODECL(jaspQmlSource_Interface)


#endif // JASPQMLSOURCE_H
