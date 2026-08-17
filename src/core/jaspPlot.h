#pragma once

// CORE (R-free) version of jaspPlot.h. Plot object payloads travel through the
// jaspHost object store as opaque std::any handles. Rendering is delegated to
// jaspHost::renderPlot (R adapter implements the legacy tryToWriteImage path;
// Python adapter will implement plotly/matplotlib rendering). The R-facing
// jaspPlot_Interface lives in src/adapters/rcpp/rcppInterfaces.h.

#include "jaspObject.h"
#include "jaspHost.h"
#include <any>

class jaspPlot : public jaspObject
{
public:
	jaspPlot(std::string title = "") : jaspObject(jaspObjectType::plot, title) { initEnvName(); }

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

	///Machine-readable data exported by analysis authors for consumers
	///like RoboReport (e.g., median effect size, credible intervals,
	///BF at specific prior widths). Survives RDS stripping because it's
	///a plain JSON value, not an environment or ggplot object.
	Json::Value _export = Json::nullValue;

	///For safekeeping (aka state replacement?)
	void setPlotObject(std::any plotSerialized);
	void renderPlot();

	std::string dataToString(std::string prefix)			const	override;

	Json::Value	metaEntry()									const	override { return constructMetaEntry("image"); }
	Json::Value	dataEntry(std::string & errorMessage)		const	override;
	std::string toHtml()									const	override;

	Json::Value convertToJSON()								const	override;
	void		convertFromJSON_SetFields(Json::Value in)			override;

	bool		canShowErrorMessage()						const	override { return true; }

	void		complete()	override	{ if(_status == "running" || _status == "waiting") _status = "complete"; }
	void		letRun()	override	{ _status = "running"; }

private:
	void initEnvName();

	//Rcpp::Vector<RAWSXP> _plotObjSerialized;
};
