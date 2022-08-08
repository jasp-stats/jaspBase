#pragma once
#include "jaspContainer.h"

#ifdef JASP_R_INTERFACE_LIBRARY
#include "jasprcpp_interface.h"

#define GUARD_ENCODE_FUNCS(...) throw std::runtime_error("Do not use column encoding from jaspResults when running in JASP!");
#else
typedef void (*sendFuncDef)(const char *);
typedef bool (*pollMessagesFuncDef)();
//If you edit any function(signatures) or objects for JASP* Rcpp modules and you want to run it as an R-Package you should run Rcpp::compileAttributes from an R instance started in $PWD/R-Interface/jaspResults

#include "columnencoder.h"
#define INCLUDE_COLUMN_ENCODING_ETC
#define GUARD_ENCODE_FUNCS(CODE) CODE
#endif


																									\

class jaspResults : public jaspContainer
{
public:
	jaspResults(Rcpp::String title, Rcpp::RObject oldState);
	~jaspResults();

	//static functions to allow the values to be set before the constructor is called from R. Would be nicer to just run the constructor in C++ maybe?
	static void			setSendFunc(sendFuncDef sendFunc);
	static void			setSendFuncXPtr(Rcpp::XPtr<sendFuncDef> sendFunc);
	static void			setPollMessagesFunc(pollMessagesFuncDef pollFunc);
	static void			setPollMessagesFuncXPtr(Rcpp::XPtr<pollMessagesFuncDef> pollFunc);
	static void			setResponseData(int analysisID, int revision);
	static void			setSaveLocation(const std::string & root, const std::string & relativePath);
	static void			setWriteSealLocation(const std::string & root, const std::string & relativePath);
	static void			setBaseCitation(std::string baseCitation);
	static void			setInsideJASP();
	static bool			isInsideJASP() { return _insideJASP; }
	static Rcpp::String	writeSealFilename() { return "jaspResultsFinishedWriting.txt"; }

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
	void			complete();

	void			prepareForWriting();
	void			finishWriting();
	bool			lastWriteWorked() const;
	void			saveResults();

	void			loadResults();
	void			setErrorMessage(Rcpp::String msg, std::string errorStatus);
	void			changeOptions(std::string opts);
	void			setOptions(std::string opts);
	void			pruneInvalidatedData();

	Rcpp::List		getOtherObjectsForState();
	Rcpp::List		getPlotObjectsForState();
	Rcpp::List		getPlotPathsForKeep();
	Rcpp::List		getKeepList();
	std::string		getResults() { return constructResultJson(); }

	std::string _relativePathKeep;

	Json::Value convertToJSON()								const	override;
	void		convertFromJSON_SetFields(Json::Value in)			override;


	void startProgressbar(int expectedTicks, Rcpp::String label);
	void progressbarTick();

	static void staticStartProgressbar(int expectedTicks, Rcpp::String label)			{ _jaspResults->startProgressbar(expectedTicks, label); }
	static void staticProgressbarTick()													{ _jaspResults->progressbarTick(); }

	static Rcpp::RObject	getObjectFromEnv(std::string envName);
	static void				setObjectInEnv(std::string envName, Rcpp::RObject obj);
	static bool				objectExistsInEnv(std::string envName);

	jaspContainer *			getOldResults()		const	{ return _oldResults; }

	jaspObject *			getOldObjectFromUniqueNestedNameVector(const std::vector<std::string>& uniqueNames)	override { return _oldResults == nullptr ? nullptr : _oldResults->findObjectWithNestedNameVector(uniqueNames); } ;


	void	setCurrentNamesFromOptionsMeta(Json::Value & optionsMeta)		{ GUARD_ENCODE_FUNCS(_extraEncodings->setCurrentNamesFromOptionsMeta(optionsMeta);	) }
	void	setCurrentColumnNames(const std::vector<std::string> & names) 	{ GUARD_ENCODE_FUNCS(ColumnEncoder::setCurrentColumnNames(names); 					) }

	std::string encodeColumnName(const std::string & in)	{ GUARD_ENCODE_FUNCS(return _extraEncodings->shouldEncode(in) ? _extraEncodings->encode(in) : ColumnEncoder::columnEncoder()->encode(in); ) }
	std::string decodeColumnName(const std::string & in)	{ GUARD_ENCODE_FUNCS(return _extraEncodings->shouldEncode(in) ? _extraEncodings->decode(in) : ColumnEncoder::columnEncoder()->decode(in); ) }
	std::string encodeAllColumnNames(const std::string & in){ GUARD_ENCODE_FUNCS(return ColumnEncoder::columnEncoder()->encodeAll(in); ) }
	std::string decodeAllColumnNames(const std::string & in){ GUARD_ENCODE_FUNCS(return ColumnEncoder::columnEncoder()->decodeAll(in); ) }

private:

	// silences e.g., "./jaspResults.h:36:15: warning: 'jaspResults::dataEntry' hides overloaded virtual function [-Woverloaded-virtual]"
	Json::Value	metaEntry(jaspObject * )					const	override { throw std::runtime_error("Don't call jaspResults::metaEntry(jaspObject * oldResult)"); };
	Json::Value	dataEntry(jaspObject *, std::string & )		const	override { throw std::runtime_error("Don't call jaspResults::dataEntry(jaspObject * oldResult, std::string & errorMsg)"); };

	static jaspResults				*	_jaspResults;
	static Rcpp::Environment		*	_RStorageEnv; //we need this environment to store R objects in a "named" fashion, because then the garbage collector doesn't throw away everything...
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

	void addSerializedPlotObjsForStateFromJaspObject(	jaspObject * obj, Rcpp::List & pngImgObj);
	void addPlotPathsForKeepFromJaspObject(				jaspObject * obj, Rcpp::List & pngPathImgObj);
	void addSerializedOtherObjsForStateFromJaspObject(	jaspObject * obj, Rcpp::List & cumulativeList);
	void fillEnvironmentWithStateObjects(Rcpp::List state);
	void storeOldResults();


	int		_progressbarExpectedTicks		= 100,
			_progressbarLastUpdateTime		= -1,
			_progressbarTicks				= 0,
			_sendingFeedbackLastTime		= -1,
			_progressbarBetweenUpdatesTime	= 500,
			_sendingFeedbackInterval		= 1000;

#ifdef INCLUDE_COLUMN_ENCODING_ETC
	ColumnEncoder	* _extraEncodings		= nullptr;
#endif


};

void JASPresultFinalizer(jaspResults * obj);

 Rcpp::RObject givejaspResultsModule();


class  jaspResults_Interface : public jaspContainer_Interface
{
public:
	jaspResults_Interface(jaspObject * dataObj) : jaspContainer_Interface(dataObj) {}

	void		send()								{ ((jaspResults*)myJaspObject)->send();								}
	void		complete()							{ ((jaspResults*)myJaspObject)->complete();							}
	void		saveResults()						{ ((jaspResults*)myJaspObject)->saveResults();						}
	void		finishWriting()						{ ((jaspResults*)myJaspObject)->finishWriting();					}
	Rcpp::List	getOtherObjectsForState()			{ return ((jaspResults*)myJaspObject)->getOtherObjectsForState();	}
	Rcpp::List	getPlotObjectsForState()			{ return ((jaspResults*)myJaspObject)->getPlotObjectsForState();	}
	Rcpp::List	getKeepList()						{ return ((jaspResults*)myJaspObject)->getKeepList();				}
	std::string getResults()						{ return ((jaspResults*)myJaspObject)->getResults();				}

	void		setErrorMessage(Rcpp::String msg, std::string errorStatus)			{ ((jaspResults*)myJaspObject)->setErrorMessage(msg, errorStatus);							}

	void		setOptions(std::string opts)		{ ((jaspResults*)myJaspObject)->setOptions(opts); }
	void		changeOptions(std::string opts)		{ ((jaspResults*)myJaspObject)->changeOptions(opts); }

	void		setStatus(std::string status)		{ ((jaspResults*)myJaspObject)->setStatus(status); }
	std::string getStatus()							{ return ((jaspResults*)myJaspObject)->getStatus(); }

	void		prepareForWriting()					{ ((jaspResults*)myJaspObject)->prepareForWriting(); }

	void	setCurrentColumnNames(Rcpp::CharacterVector names)
	{
		std::vector<std::string> vec;

		for(int row=0; row<names.size(); row++)
			vec.push_back((std::string)(names[row]));

		((jaspResults*)myJaspObject)->setCurrentColumnNames(vec);
	}

	std::string encodeColumnName(	 const std::string & in) {return ((jaspResults*)myJaspObject)->encodeColumnName(in);	 }
	std::string decodeColumnName(	 const std::string & in) {return ((jaspResults*)myJaspObject)->decodeColumnName(in);	 }
	std::string encodeAllColumnNames(const std::string & in) {return ((jaspResults*)myJaspObject)->encodeAllColumnNames(in); }
	std::string decodeAllColumnNames(const std::string & in) {return ((jaspResults*)myJaspObject)->decodeAllColumnNames(in); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspResults, std::string,	_relativePathKeep, RelativePathKeep)
};


RCPP_EXPOSED_CLASS_NODECL(jaspResults_Interface)
