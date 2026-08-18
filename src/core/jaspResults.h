#pragma once

// CORE (R-free) version of jaspResults. The R storage environment, RDS save,
// Rcpp::List state harvest, XPtr send/poll unwrapping and the R-facing
// jaspResults_Interface live in src/adapters/rcpp/rcppResults. The object
// store is reached through jaspHost (R build wires it to the R environment).

#include "jaspContainer.h"
#include "jaspHost.h"

//copied from jasprcpp_interface.h
typedef void (*sendFuncDef)(const char *);
typedef bool (*pollMessagesFuncDef)();

///Neutral plot-state harvest entry. The R adapter rebuilds today's
///Rcpp::List shape (obj/width/height/revision/envName/getUnique) from these.
struct jaspPlotStateEntry
{
	std::string filePathPng, envName, uniqueNestedName;
	int width = 0, height = 0, revision = 0;
};

class jaspResults : public jaspContainer
{
public:
	jaspResults(std::string title);
	~jaspResults();

	//static functions to allow the values to be set before the constructor is called from R. Would be nicer to just run the constructor in C++ maybe?
	static void			setSendFunc(sendFuncDef sendFunc);
	static void			setPollMessagesFunc(pollMessagesFuncDef pollFunc);
	static void			setResponseData(int analysisID, int revision);
	static void			setSaveLocation(const std::string & root, const std::string & relativePath);
	static void			setWriteSealLocation(const std::string & root, const std::string & relativePath);
	static void			setBaseCitation(std::string baseCitation);
	static void			setInsideJASP();
	static bool			isInsideJASP() { return _insideJASP; }
	static std::string	writeSealFilename() { return "jaspResultsFinishedWriting.txt"; }

	void			send(std::string otherMsg = "");
	void			checkForAnalysisChanged();
	void			setStatus(std::string status);
	std::string		getStatus();

	const char *	constructResultJson();
	Json::Value		metaEntry()								const override;
	Json::Value		dataEntry(std::string & errorMessage)	const override;
	Json::Value		dataEntry()								const			{ std::string dummy(""); return dataEntry(dummy); }

	void			childrenUpdatedCallbackHandler(bool ignoreSendTimer) override;

	void			finalizedHandler() override { complete(); }
	void			complete() override;

	void			prepareForWriting();
	void			finishWriting();
	bool			lastWriteWorked() const;
	void			saveResults();

	void			loadResults();
	void			setErrorMessage(std::string msg, std::string errorStatus);
	void			changeOptions(std::string opts);
	void			setOptions(std::string opts);
	void			pruneInvalidatedData();

	///Neutral state harvests (host adapters convert to their native shapes).
	std::vector<jaspPlotStateEntry>	harvestPlotObjects();
	std::vector<std::string>		harvestStateEnvNames();
	std::vector<std::string>		harvestPlotPathsForKeep();
	std::vector<std::string>		getKeepListVector();

	std::string		getResults() { return constructResultJson(); }

	std::string _relativePathKeep;

	Json::Value convertToJSON()								const	override;
	void		convertFromJSON_SetFields(Json::Value in)			override;

	///Second half of the old constructor: load previous results from disk if
	///the last write worked. Host adapters call this after they have set up
	///the object store and filled it with old-state objects.
	void			loadResultsIfLastWriteWorked();

	void startProgressbar(int expectedTicks, std::string label);
	void progressbarTick();

	static void staticStartProgressbar(int expectedTicks, std::string label)			{ _jaspResults->startProgressbar(expectedTicks, label); }
	static void staticProgressbarTick()													{ _jaspResults->progressbarTick(); }

	static int              analysisId() { return jaspHost::analysisId(); } ///< To pass analysisId to jaspReport easily

	jaspContainer *			getOldResults()		const	{ return _oldResults; }

	jaspObject *			getOldObjectFromUniqueNestedNameVector(const std::vector<std::string>& uniqueNames)	override { return _oldResults == nullptr ? nullptr : _oldResults->findObjectWithNestedNameVector(uniqueNames); } ;

private:

	// silences e.g., "./jaspResults.h:36:15: warning: 'jaspResults::dataEntry' hides overloaded virtual function [-Woverloaded-virtual]"
	Json::Value	metaEntry(jaspObject * )					const	override { throw std::runtime_error("Don't call jaspResults::metaEntry(jaspObject * oldResult)"); };
	Json::Value	dataEntry(jaspObject *, std::string & )		const	override { throw std::runtime_error("Don't call jaspResults::dataEntry(jaspObject * oldResult, std::string & errorMsg)"); };

	static jaspResults				*	_jaspResults;
	static Json::Value					_response;
	static sendFuncDef					_ipccSendFunc;
	static pollMessagesFuncDef			_ipccPollFunc;
	static std::string					_saveResultsHere,
										_saveResultsRoot,
										_baseCitation,
										_writeSealRoot,
										_writeSealRelative;
	static bool							_insideJASP;

	std::string	errorMessage = "";
	Json::Value	_currentOptions		= Json::nullValue,
				_previousOptions	= Json::nullValue;

	jaspContainer					*	_oldResults	= nullptr;

	void addPlotStateEntriesFromJaspObject(	jaspObject * obj, std::vector<jaspPlotStateEntry> & entries);
	void addPlotPathsForKeepFromJaspObject(	jaspObject * obj, std::vector<std::string> & pngPlotPaths);
	void addStateEnvNamesFromJaspObject(	jaspObject * obj, std::vector<std::string> & envNames);
	void storeOldResults();

	static void  setAnalysisId(int analysisId);


	int		_progressbarExpectedTicks		= 100,
			_progressbarLastUpdateTime		= -1,
			_progressbarTicks				= 0,
			_sendingFeedbackLastTime		= -1,
			_progressbarBetweenUpdatesTime	= 500,
			_sendingFeedbackInterval		= 1000;
};
