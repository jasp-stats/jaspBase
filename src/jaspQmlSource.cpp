#include "jaspQmlSource.h"

jaspQmlSource::jaspQmlSource(const std::string & sourceID) : jaspObject(jaspObjectType::qmlSource, ""), _sourceID(sourceID)
{
	_escapeHtml = false;
}

Json::Value jaspQmlSource::dataEntry(std::string & errorMessage) const
{
	Json::Value dataJson(jaspObject::dataEntry(errorMessage));

	if (_complete)
	{
		dataJson["title"]		= _title;
		dataJson["json"]		= _json;
		dataJson["name"]		= getUniqueNestedName();
	}

	dataJson["sourceID"] = _sourceID;

	return dataJson;
}

void jaspQmlSource::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_json		= in.get("json", Json::nullValue);
	_sourceID	= in["sourceID"].asString();
}

Json::Value jaspQmlSource::convertToJSON() const
{
	Json::Value obj		= jaspObject::convertToJSON();
	obj["json"]			= _json;
	obj["sourceID"]		= _sourceID;

	return obj;
}

std::string jaspQmlSource::jsonToPrefixedStrings(Json::Value val, std::string prefix) const
{
	if(prefix == "")
		return val.toStyledString();

	std::string styled = val.toStyledString();

	std::stringstream out;

	for(char letter : styled)
		if(letter != '\n')
			out << letter;
		else
			out << letter << prefix;

	return out.str();
}

bool jaspQmlSource::shouldBePartOfResultsJson(bool meta) const
{
	// If the source has not changed, send the meta part, but not the result
	return jaspObject::shouldBePartOfResultsJson(meta) && (meta || changed());
}
