#pragma once
#include "jaspObject.h"

class jaspPlot : public jaspObject
{
public:
	jaspPlot(Rcpp::String title = "") : jaspObject(jaspObjectType::plot, title) { initEnvName(); }

	~jaspPlot();

	float		_aspectRatio;
	int			_width,
				_height,
				_revision = 0;
	bool		_editing = false,
				_resizedByUser = false,
				_interactive = false;
	std::string	_filePathPng,
				_status = "waiting",
				_envName,
				_interactiveConvertError = "",
				_interactiveJsonData = "";
	Json::Value _editOptions = Json::nullValue;

	///For safekeeping (aka state replacement?)
	void setPlotObject(Rcpp::RObject plotSerialized);
	void renderPlot();
	Rcpp::RObject getPlotObject()							const;

	std::string dataToString(std::string prefix)			const	override;

	Json::Value	metaEntry()									const	override { return constructMetaEntry("image"); }
	Json::Value	dataEntry(std::string & errorMessage)		const	override;
	std::string toHtml()									const	override;

	Json::Value convertToJSON()								const	override;
	void		convertFromJSON_SetFields(Json::Value in)			override;

	bool		canShowErrorMessage()						const	override { return true; }

	void		complete()	{ if(_status == "running" || _status == "waiting") _status = "complete"; }
	void		letRun()	{ _status = "running"; }

	Rcpp::List	toRObject()									/*const*/ override;

private:
	void initEnvName();
	void setUserPlotChangesFromRStateObject();

	Rcpp::List getOldPlotInfo(Rcpp::List & plotInfo);

	//Rcpp::Vector<RAWSXP> _plotObjSerialized;
};


class jaspPlot_Interface : public jaspObject_Interface
{
public:
	jaspPlot_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

	void setPlotObject(Rcpp::RObject plotObject)					{ ((jaspPlot*)myJaspObject)->setPlotObject(plotObject); }
	Rcpp::RObject getPlotObject()									{ return ((jaspPlot*)myJaspObject)->getPlotObject(); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, std::string,	_filePathPng,	FilePathPng)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, std::string,	_status,		Status)

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, float,			_aspectRatio,	AspectRatio)

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, int,			_width,			Width)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, int,			_height,		Height)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspPlot, int,			_revision,		Revision)

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NO_NOTIFY(jaspPlot, bool,	_editing,		Editing)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NO_NOTIFY(jaspPlot, bool,	_resizedByUser,	ResizedByUser)
};

RCPP_EXPOSED_CLASS_NODECL(jaspPlot_Interface)
