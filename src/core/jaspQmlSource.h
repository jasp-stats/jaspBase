#ifndef JASPQMLSOURCE_H
#define JASPQMLSOURCE_H

// CORE (R-free) version of jaspQmlSource.h. setValue() takes Json::Value (the
// Rcpp conversion happens in jaspQmlSource_Interface); the unused Rcpp-only
// RcppVector_to_ArrayJson helper was dropped. jaspQmlSource_Interface moved to
// src/adapters/rcpp/rcppInterfaces.h.

#include "jaspObject.h"

class jaspQmlSource : public jaspObject
{
public:
					jaspQmlSource(const std::string & sourceID = "");

	void			setSourceID(const std::string & sourceID)							{ _sourceID = sourceID; }
	std::string		sourceID()										const;
	void			setValue(Json::Value json)											{ _json = json; _changed = true;	}
	std::string		getValue()										const				{ return _json.toStyledString();		}

	Json::Value		metaEntry()										const	override;
	Json::Value		dataEntry(std::string & errorMessage)			const	override;

	void			convertFromJSON_SetFields(Json::Value in)				override;
	Json::Value		convertToJSON()									const	override;

	std::string		dataToString(std::string prefix)				const	override	{ return jsonToPrefixedStrings(prefix + "\t"); }
	std::string		jsonToPrefixedStrings(std::string prefix = "")	const				{ return jsonToPrefixedStrings(_json, prefix); }
	std::string		jsonToPrefixedStrings(Json::Value val, std::string prefix) const;

	bool			shouldBePartOfResultsJson(bool meta = false)	const	override;

	void			complete()	{ _complete = true; }
	bool			changed()										const				{ return _changed; }
	std::string		_sourceID;

protected:
	Json::Value		_json;
	bool			_complete	= false, ///<- This is used to keep the logfiles/resultjson small until the source is actually needed. Which is at complete only anyway
					_changed	= false;

};

#endif // JASPQMLSOURCE_H
