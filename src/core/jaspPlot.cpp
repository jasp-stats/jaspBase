// CORE (R-free) version of jaspPlot.cpp.
// Rendering is delegated to jaspHost::renderPlot; the R adapter supplies the
// original tryToWriteImage logic (see adapters/rcpp/rcppPlot.cpp).

#include "jaspPlot.h"

jaspPlot::~jaspPlot()
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	jaspPrint("Destructor of JASPplot("+_title+") is called! ");
#endif

	finalizedHandler();
}

std::string jaspPlot::dataToString(std::string prefix) const
{
	std::stringstream out;

	out <<
		prefix << "aspectRatio: "	<< _aspectRatio << "\n" <<
		prefix << "dims:        "	<< _width << "X" << _height << "\n" <<
		prefix << "error:       '"	<< _error << "': '" << _errorMessage << "'\n" <<
		prefix << "filePath:    "	<< _filePathPng << "\n" <<
		prefix << "status:      "	<< _status << "\n" ;

	return out.str();
}

Json::Value jaspPlot::dataEntry(std::string & errorMessage) const
{
	Json::Value data(jaspObject::dataEntry(errorMessage));

	data["title"]				= _title;
	data["convertible"]			= true;
	data["data"]				= _filePathPng;
	data["height"]				= _height;
	data["width"]				= _width;
	data["aspectRatio"]			= _aspectRatio;
	data["status"]				= _error ? "error" : _status;
	data["revision"]			= _revision;
	data["name"]				= getUniqueNestedName();
	data["editOptions"]			= _editOptions;
	data["reasonNotEditable"]	= _editOptions.get("reasonNotEditable", "unknown reason");
	data["errorType"]			= _editOptions.get("errorType", "fatalError");
	data["editable"]			= !_editOptions.isNull() && data["errorType"] == "success";

	data["interactive"]				= _interactive;
	data["interactiveConvertError"]	= _interactiveConvertError;
	data["interactiveJsonData"]		= _interactiveJsonData;

	data["export"]					= _export;

	return data;
}

void jaspPlot::initEnvName()
{
	static int counter = 0;

	_envName = "plot_" + std::to_string(counter++);
}

void jaspPlot::setPlotObject(std::any plotSerialized)
{
	if (!_editing)
		_filePathPng = "";

	jaspHost::storeObject(_envName, std::move(plotSerialized));

	if (connectedToJaspResults())
		renderPlot();
}

void jaspPlot::renderPlot()
{
	if (jaspHost::renderPlot)
		jaspHost::renderPlot(*this);
}

Json::Value jaspPlot::convertToJSON() const
{
	Json::Value obj		= jaspObject::convertToJSON();

	obj["aspectRatio"]			= _aspectRatio;
	obj["width"]				= _width;
	obj["height"]				= _height;
	obj["status"]				= _status;
	obj["filePathPng"]			= _filePathPng;
	obj["revision"]				= _revision;
	obj["environmentName"]		= _envName;
	obj["editOptions"]			= _editOptions;
	obj["resizedByUser"]		= _resizedByUser;

	obj["interactive"]				= _interactive;
	obj["interactiveConvertError"]	= _interactiveConvertError;
	obj["interactiveJsonData"]		= _interactiveJsonData;

	obj["export"]					= _export;

	return obj;
}

void jaspPlot::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);

	_aspectRatio	= in.get("aspectRatio",		0.0f).asDouble();
	_width			= in.get("width",			-1).asInt();
	_height			= in.get("height",			-1).asInt();
	_revision		= in.get("revision", 		0).asInt();
	_status			= in.get("status",			"complete").asString();
	_filePathPng	= in.get("filePathPng",		"null").asString();
	_envName		= in.get("environmentName",	_envName).asString();
	_editOptions	= in.get("editOptions",		Json::nullValue);
	_resizedByUser	= in.get("resizedByUser",	false).asBool();

	_interactive				= in.get("interactive", 				false).asBool();
	_interactiveConvertError	= in.get("interactiveConvertError", 	"").asString();
	_interactiveJsonData		= in.get("interactiveJsonData", 		"").asString();

	_export						= in.get("export",					Json::nullValue);

	if (jaspHost::plotStateSync)
		jaspHost::plotStateSync(*this);
}

std::string jaspPlot::toHtml() const
{
	std::stringstream out;

	out << "<div class=\"status " << _status << "\">" "\n"
		<< htmlTitle() << "\n";

	if(_error || _errorMessage != "")
	{
		out << "<p class=\"error\">\n";
		if(_error		      ) out << "error: <i>'" << _error << "'</i>";
		if(_errorMessage != "") out << (_error       ? " msg: <i>'" : "errormessage: <i>'") << _errorMessage << "'</i>";
		out << "\n</p>";
	}
	else
		out << "<img src=\"" << _filePathPng << "\" height=\"" << _height << "\" width=\"" << _width << "\" alt=\"a plot called " << _title << "\">";

	out << "</div>\n";

	return out.str();
}
