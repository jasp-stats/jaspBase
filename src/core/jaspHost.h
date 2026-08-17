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

class jaspPlot;

class jaspHost
{
public:
	// messaging / engine loop -------------------------------------------------
	static std::function<void(const std::string &)>			logString;				///< sink for jaspPrint
	static std::function<void(const char *)>				sendResults;			///< results JSON to the desktop
	static std::function<bool()>							pollMessages;			///< true => analysis changed
	static std::function<void()>							signalAnalysisAbort;
	static std::function<std::string(const std::string &)>	decodeColumnNames;		///< default: identity

	// identity of the running analysis (set from setResponseData). Used by
	// jaspReport (positioning) and jaspColumn (ownership) without needing the
	// jaspResults object. -1 == none/unknown.
	static int	analysisId();
	static void	setAnalysisId(int id);

	// object store (plot + state objects), keyed by envName. Default is a plain
	// in-process map; hosts can override, e.g. the R engine stores objects in an
	// R environment so R's GC keeps them alive (installed by jaspResults).
	static std::function<void(const std::string &, std::any)>	storeObject;
	static std::function<std::any(const std::string &)>			fetchObject;
	static std::function<bool(const std::string &)>				objectExists;
	static std::function<void()>								clearObjects;

	// plotting ------------------------------------------------------------------
	// renderPlot owns the whole render pass (the R adapter reproduces the old
	// tryToWriteImageJaspResults flow incl. old-plot info; the Python adapter
	// will render plotly/matplotlib). plotStateSync re-applies user plot changes
	// (width/height/revision) from the stored object after convertFromJSON.
	static std::function<void(jaspPlot &)> renderPlot;
	static std::function<void(jaspPlot &)> plotStateSync;

	// state archive (R: RDS alongside jaspResults.json; Python: pickle) ---------
	static std::function<void(const std::string & path)> saveStateArchive;

private:
	static int _analysisId;
};
