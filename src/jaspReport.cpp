#include "jaspReport.h"
#include "jaspHtml.h"

std::string jaspReport::dataToString(std::string prefix) const
{
    return jaspHtml::sanitizeTextForHtml(_rawText);
}

std::string jaspReport::toHtml() const
{
	//we add two classes, one to indicate it is a jaspReport (and to easily set display:none/block and jaspReportDo(nt) to indicate whether the user should be alerted or not
	return std::string("<div class=\"jaspReport jaspReport") + 
		(_report ? "Do" : "Dont") + "\">"
		"<div class=\"jaspReportTitleRow\"> <div class=\"jaspReportIcon jaspReportIcon" + (_report ? "Do" : "Dont")  +"\" alt=\"" + (_report ? "Warning: " : "Info: ") + "\"/>"
		"<h1 class=\"jaspReportTitle\">" + _title + "</h1></div>"
		"<p>" + jaspHtml::sanitizeTextForHtml(_rawText) + "</p></div>";
}

Json::Value jaspReport::dataEntry(std::string & errorMessage) const
{
	Json::Value data(jaspObject::dataEntry(errorMessage));

    data["rawtext"]			= _rawText;
    data["html"]			= toHtml();
	data["title"]			= "hide me"; //the real title is in ::toHtml()
	data["name"]			= getUniqueNestedName();
    data["report"]			= _report;

	return data;
}


Json::Value jaspReport::convertToJSON() const
{
	Json::Value obj		= jaspObject::convertToJSON();
    obj["rawtext"]		= _rawText;
	obj["report"]		= _report;

	return obj;
}

void jaspReport::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

    _rawText	= in.get("rawtext",	"null").asString();
	_report		= in.get("report",	false).asBool();
}

