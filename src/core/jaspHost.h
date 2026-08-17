#pragma once

// jaspHost: the language/host seam of the jaspResults core.
//
// Everything that used to be hardwired to R (logging, sending results to the
// desktop, polling for analysis changes, abort signalling, column-name
// decoding, the plot/state object store, plot rendering and state-file
// saving) becomes a callback or a store on this class. The R engine installs
// R-backed implementations (src/adapters/rcpp); a Python host will install
// Python-backed ones (see tmp/plan-python-interface.md section 2.1).
//
// Defaults are neutral no-ops (identity for decodeColumnNames) so the core is
// usable standalone, e.g. in tests. All callbacks are expected to be set
// before an analysis runs, mirroring today's engine flow where setSendFunc &
// friends are called before the analysis starts. Single-threaded use, like
// today's engine.

#include <any>
#include <functional>
#include <string>

struct jaspPlotRenderRequest
{
	std::string envName;				///< key of the plot object in the object store
	std::string pngRelativePathIn,		///< set when editing an existing plot
				interactiveJsonPathIn;
	int			width = 0, height = 0;
	float		aspectRatio = 0.f;
	bool		editing = false;
};

struct jaspPlotRenderResult
{
	std::string pngRelativePath,
				editOptionsJson,
				interactiveConvertError,
				interactiveJsonRelativePath;
	bool		interactive = false;
};

class jaspHost
{
public:
	// messaging / engine loop -------------------------------------------------
	static std::function<void(const std::string &)>			logString;				///< sink for jaspPrint
	static std::function<void(const char *)>				sendResults;			///< results JSON to the desktop
	static std::function<bool()>							pollMessages;			///< true => analysis changed
	static std::function<void()>							signalAnalysisAbort;
	static std::function<std::string(const std::string &)>	decodeColumnNames;		///< default: identity

	// object store (plot + state objects), keyed by envName --------------------
	static void			storeObject(const std::string & envName, std::any obj);
	static std::any		fetchObject(const std::string & envName);
	static bool			objectExists(const std::string & envName);
	static void			clearObjects();

	// plotting ------------------------------------------------------------------
	static std::function<jaspPlotRenderResult(const jaspPlotRenderRequest &)> renderPlot;

	// state archive (R: RDS alongside jaspResults.json; Python: pickle) ---------
	static std::function<void(const std::string & path)> saveStateArchive;
};
