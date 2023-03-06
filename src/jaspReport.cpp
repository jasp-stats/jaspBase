#include "jaspReport.h"
#include "jaspHtml.h"
#include "jaspResults.h"

size_t  jaspReport::_totalWarnings = 0;

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

std::string jaspReport::toTopHtml() const
{
	if(!_report)
		return "";

	//Simply put ~ZZZ~ and replace it in the results (because analyses might be moved around)
	return	"<div class=\"jaspReportTop jaspReportDo\" style=\"z-index: ~ZZZ~;\">"
			""
            "<div class=\"jaspReportTitleRow\"><div class=\"jaspReportIconTop jaspReportIconDo\"/></div></div>";
}


Json::Value jaspReport::dataEntry(std::string & errorMessage) const
{
	Json::Value data(jaspObject::dataEntry(errorMessage));

    data["rawtext"]			= _rawText;
	data["html"]			= toHtml();
	data["topHtml"]			= toTopHtml();
	data["title"]			= "hide me"; //the real title is in ::toHtml()
	data["name"]			= getUniqueNestedName();
    data["report"]			= _report;
	data["warningIndex"]	= int(_warningIndex);
	data["warningsTotal"]	= int(_totalWarnings);
    data["analysisId"]		= jaspResults::analysisId(); //Used to find analysis position in js and reposition topnodes

	return data;
}

Json::Value jaspReport::convertToJSON() const
{
	Json::Value obj		= jaspObject::convertToJSON();
    obj["rawtext"]		= _rawText;
	obj["report"]		= _report;
	obj["warningIndex"] = int(_warningIndex);
	obj["warnings"]		= int(_totalWarnings); // Doesnt really make sense as it is static, but at least it will all be the same value :p

	return obj;
}

void jaspReport::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_rawText		= in.get("rawtext",			"null").asString();
	_report			= in.get("report",			false).	asBool();
	_warningIndex	= in.get("warningIndex",	0).		asUInt();
	_totalWarnings	= in.get("warnings",		0).		asUInt();
}

