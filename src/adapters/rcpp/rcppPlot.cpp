// R-backed plot logic moved from the old src/jaspPlot.cpp. Behaviour is kept
// identical: the plot object (an R list with obj/width/height/revision) lives
// in the jaspHost object store, which in the R build is backed by
// jaspResults::_RStorageEnv.

#include "rcppPlot.h"
#include "jaspPlot.h"
#include "jaspHost.h"
#include "jaspResults.h"
#include "rcppConversions.h"
#include "rcppInterfaces.h"

static Rcpp::RObject anyToRObject(const std::any & obj)
{
	if(!obj.has_value())
		return R_NilValue;

	try
	{
		return std::any_cast<Rcpp::RObject>(obj);
	}
	catch(const std::bad_any_cast &)
	{
		return R_NilValue;
	}
}

Rcpp::RObject rcppGetPlotObjectFromEnvName(const std::string & envName)
{
	Rcpp::RObject plotInfoObj = anyToRObject(jaspHost::fetchObject(envName));

	if (!plotInfoObj.isNULL() && Rcpp::is<Rcpp::List>(plotInfoObj))
	{

		Rcpp::List plotInfoList = Rcpp::as<Rcpp::List>(plotInfoObj);
		if (plotInfoList.containsElementNamed("obj"))
			return Rcpp::as<Rcpp::RObject>(plotInfoList["obj"]);

	}
	return R_NilValue;
}

Rcpp::RObject rcppGetPlotObject(jaspPlot * plot)
{
	return rcppGetPlotObjectFromEnvName(plot->_envName);
}

void rcppSetUserPlotChangesFromRStateObject(jaspPlot & plot)
{
	Rcpp::RObject plotInfoObj = anyToRObject(jaspHost::fetchObject(plot._envName));

	if (plotInfoObj.isNULL() || !Rcpp::is<Rcpp::List>(plotInfoObj))
		return;

	Rcpp::List plotInfoList = Rcpp::as<Rcpp::List>(plotInfoObj);

	if (plotInfoList.containsElementNamed("width"))
		plot._width = Rcpp::as<int>(plotInfoList["width"]);

	if (plotInfoList.containsElementNamed("height"))
		plot._height = Rcpp::as<int>(plotInfoList["height"]);

	if (plotInfoList.containsElementNamed("revision"))
		plot._revision = Rcpp::as<int>(plotInfoList["revision"]);
}

static Rcpp::List rcppGetOldPlotInfo(jaspPlot & plot, Rcpp::List & plotInfo)
{
	std::vector<std::string> names;
	plot.getUniqueNestedNameVector(names);
	jaspPlot * oldPlot = dynamic_cast<jaspPlot *>(plot.getOldObjectFromUniqueNestedNameVector(names));

	if (oldPlot == nullptr)
	{
		jaspPrint("could not find an old plot");
		return Rcpp::List();
	}
	jaspPrint("found a " + oldPlot->type() + " with name: " + oldPlot->name() + ". Resized by user: " + (oldPlot->_resizedByUser ? "yes" : "no"));

	if (oldPlot->_resizedByUser)
	{
		plot._width			= oldPlot->_width;
		plot._height		= oldPlot->_height;
		plotInfo["width"]	= plot._width;
		plotInfo["height"]	= plot._height;
	}

	if (oldPlot->_editOptions == Json::nullValue)
		return Rcpp::List();
	else
		return	Rcpp::List::create(
					Rcpp::_["editOptions"]	= Rcpp::String(oldPlot->_editOptions.toStyledString()),
					Rcpp::_["oldPlot"]		= rcppGetPlotObject(oldPlot)
				);

}

void rcppRenderPlot(jaspPlot & plot)
{
	// if a png exists the plot was already rendered, unless we're editing it
	if (plot._filePathPng != "" && !plot._editing)
		return;

	// empty plots were added to the state
	Rcpp::RObject plotInfoObj = anyToRObject(jaspHost::fetchObject(plot._envName));
	if (plotInfoObj.isNULL())
		return;

	Rcpp::List plotInfo = Rcpp::as<Rcpp::List>(plotInfoObj);
	Rcpp::RObject obj = plotInfo["obj"];

	if(!obj.isNULL())
	{

		jaspPrint("Now rendering a plot with name: " + plot.name());

		static Rcpp::Function tryToWriteImage = Rcpp::Environment::namespace_env("jaspBase")["tryToWriteImageJaspResults"];
		Rcpp::List writeResult, oldPlotInfo;
		if (plot._editing)
		{
			oldPlotInfo = Rcpp::List();
			plot._revision++;
			writeResult = tryToWriteImage(Rcpp::_["width"] = plot._width, Rcpp::_["height"] = plot._height, Rcpp::_["plot"] = obj, Rcpp::_["oldPlotInfo"] = oldPlotInfo, Rcpp::_["relativePathpng"] = plot._filePathPng, Rcpp::_["relativePathJson"] = Rcpp::String(plot._interactiveJsonData));
		}
		else
		{
			//getOldPlotInfo may update height & width
			oldPlotInfo = rcppGetOldPlotInfo(plot, plotInfo);
			writeResult = tryToWriteImage(Rcpp::_["width"] = plot._width, Rcpp::_["height"] = plot._height, Rcpp::_["plot"] = obj, Rcpp::_["oldPlotInfo"] = oldPlotInfo, Rcpp::_["relativePathpng"] = R_NilValue);
		}

		// we need to overwrite plot functions with their recordedplot result
		if(Rcpp::is<Rcpp::Function>(obj) && writeResult.containsElementNamed("obj"))
			plotInfo["obj"] = writeResult["obj"];

		if(writeResult.containsElementNamed("png"))
			plot._filePathPng = Rcpp::as<Rcpp::String>(writeResult["png"]);

		plot._editOptions = Json::nullValue;

		if(writeResult.containsElementNamed("editOptions") && !Rf_isNull(writeResult["editOptions"]))
		{
			std::string editOptionsStr = Rcpp::as<Rcpp::String>(writeResult["editOptions"]);

			if(editOptionsStr != "")
			{
				plot._editOptions = Json::objectValue;
				Json::Reader().parse(editOptionsStr, plot._editOptions);
			}
		}

		if(writeResult.containsElementNamed("interactive"))
		{
			plot._interactive = Rcpp::as<bool>(writeResult["interactive"]);
			if (plot._interactive)
			{
				if(writeResult.containsElementNamed("interactiveConvertError"))
				{
					plot._interactiveConvertError = Rcpp::as<std::string>(writeResult["interactiveConvertError"]);
					plot._interactiveJsonData = "";
				}
				else if (writeResult.containsElementNamed("interactiveJsonData"))
				{
					std::string interactiveJsonDataStr = Rcpp::as<std::string>(writeResult["interactiveJsonData"]);
					plot._interactiveJsonData = interactiveJsonDataStr;
					plot._interactiveConvertError = "";
				}
				else
					plot._interactiveConvertError = "Unknown error converting interactive plot to JSON";
			}
		}


		if(writeResult.containsElementNamed("error"))
			plot.setError(Rcpp::as<Rcpp::String>(writeResult["error"]));
		else
			plot.clearError();

		plot.complete();

		jaspHost::storeObject(plot._envName, std::any((Rcpp::RObject)plotInfo));
	}
}

Rcpp::List rcppPlotToRObject(jaspPlot * plot)
{
	Rcpp::List lst = Rcpp::List::create(Rcpp::Named("plotObject") = rcppGetPlotObject(plot));
	lst.attr("title") = plot->_title;
	lst.attr("class") = Rcpp::CharacterVector({"jaspPlotWrapper", "jaspWrapper"});

	// Include the export field so RoboReport and other RDS consumers can
	// access machine-readable data (e.g., computed effect sizes) that the
	// analysis author tagged onto the plot. Survives RDS stripping.
	if (!plot->_export.isNull())
	{
		static Rcpp::Function fromJSON_export = Rcpp::Environment::namespace_env("jaspBase")["fromJSON"];
		Json::StreamWriterBuilder builder;
		std::string exportJson = Json::writeString(builder, plot->_export);
		lst["export"] = Rcpp::as<Rcpp::List>(fromJSON_export(exportJson));
	}

	// the reason this function is not const
	Rcpp::Environment jaspObjectEnvironment = Rcpp::new_env();
	jaspObjectEnvironment.assign("jaspObject", Rcpp::as<Rcpp::RObject>(Rcpp::wrap(jaspPlot_Interface(plot))));
	lst.attr("jaspObjectEnvironment") = jaspObjectEnvironment;

	return lst;
}

// ---- jaspPlot_Interface::setExport / getExport ----
// Defined here (not inline in the header) because the conversions between
// Rcpp::List and Json::Value require non-trivial logic.

void jaspPlot_Interface::setExport(Rcpp::List exportData)
{
	jaspPlot* plot = (jaspPlot*)myJaspObject;
	plot->_export = RObject_to_JsonValue(exportData, plot->getEscapeHtml());
	myJaspObject->notifyParentOfChanges();
}

Rcpp::List jaspPlot_Interface::getExport()
{
	jaspPlot* plot = (jaspPlot*)myJaspObject;
	if (plot->_export.isNull() || plot->_export.empty())
		return Rcpp::List();

	// Convert Json::Value -> string -> R list via jsonlite (jaspBase dep).
	static Rcpp::Function fromJSON_getExport =
		Rcpp::Environment::namespace_env("jaspBase")["fromJSON"];
	Json::StreamWriterBuilder builder;
	std::string jsonStr = Json::writeString(builder, plot->_export);
	return Rcpp::as<Rcpp::List>(fromJSON_getExport(jsonStr));
}
