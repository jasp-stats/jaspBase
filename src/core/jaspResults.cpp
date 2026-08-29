// CORE (R-free) version of jaspResults.cpp.
// R-specific parts (R storage environment, RDS export, Rcpp::List state
// harvest, XPtr registration, signalAnalysisAbort R call) moved to
// src/adapters/rcpp/rcppResults.cpp.

#include <cstdlib>
#include <fstream>
#include <cmath>
#include <cstdio>

#include "jaspResults.h"
#include "jaspTable.h"
#include "jaspColumn.h"
#include "jaspHtml.h"
#include "jaspState.h"
#include "jaspPlot.h"
#include "jaspQmlSource.h"
#include "jaspReport.h"

typedef std::ofstream bofstream;
typedef std::ifstream bifstream;
#define BREMOVE std::remove //Also not a type


sendFuncDef			jaspResults::_ipccSendFunc		= nullptr;
pollMessagesFuncDef jaspResults::_ipccPollFunc		= nullptr;
std::string			jaspResults::_saveResultsHere	= "";
std::string			jaspResults::_saveResultsRoot	= "";
std::string			jaspResults::_writeSealRoot		= "";
std::string			jaspResults::_writeSealRelative	= "";
std::string			jaspResults::_baseCitation		= "";
bool				jaspResults::_insideJASP		= false;
jaspResults*		jaspResults::_jaspResults		= nullptr;

void jaspResults::setSendFunc(sendFuncDef sendFunc)
{
	_ipccSendFunc = sendFunc;
}

void jaspResults::setPollMessagesFunc(pollMessagesFuncDef pollFunc)
{
	_ipccPollFunc = pollFunc;
}


void jaspResults::setBaseCitation(std::string baseCitation)
{
	_baseCitation = baseCitation;
}

void jaspResults::setAnalysisId(int analysisId)
{
    jaspHost::setAnalysisId(analysisId);
}

void jaspResults::setResponseData(int analysisID, int revision)
{
    setAnalysisId(analysisID);

	_response["id"]			= analysisID;
	_response["revision"]	= revision;
	
	Json::Value progress;
	progress["value"]		= -1;
	progress["label"]		= "";
	_response["progress"]	= progress;
}

void jaspResults::setSaveLocation(const std::string & root, const std::string & relativePath)
{
	_saveResultsRoot	= root;
	_saveResultsHere	= relativePath;

	if(_saveResultsRoot.size() > 0 && _saveResultsRoot[_saveResultsRoot.size() - 1] != '/')
		_saveResultsRoot.push_back('/');
}

void jaspResults::setWriteSealLocation(const std::string & root, const std::string & relativePath)
{
	_writeSealRoot		= root;
	_writeSealRelative	= relativePath;

	if(_writeSealRoot.size() > 0 && _writeSealRoot[_writeSealRoot.size() - 1] != '/')
		_writeSealRoot.push_back('/');
}

void jaspResults::setInsideJASP()
{
	_insideJASP = true;
}

jaspResults::jaspResults(std::string title)
	: jaspContainer(title, jaspObjectType::results)
{
	_jaspResults = this;

	if(_insideJASP && _writeSealRoot + _writeSealRelative == "")
		throw std::runtime_error("Write seal location not given and we are running in JASP, this should never happen!");

	// The host adapter is expected to have set up its object store (R build:
	// _RStorageEnv + rcppWireHostStore, see adapters/rcpp/rcppResults.cpp) and
	// to fill it with old state objects before calling
	// loadResultsIfLastWriteWorked(), exactly as the old constructor did.

	setStatus("running");

	if(_baseCitation != "")
		addCitation(_baseCitation);
}

jaspResults::~jaspResults()
{
	if(jaspHost::destroyObjectStore)
		jaspHost::destroyObjectStore();
}

void jaspResults::loadResultsIfLastWriteWorked()
{
	if(lastWriteWorked() && _saveResultsHere != "")
		loadResults();
}

void jaspResults::setStatus(std::string status)
{
	_response["status"] = status;
}

std::string jaspResults::getStatus()
{
	return _response["status"].asString();
}

void jaspResults::prepareForWriting()
{
	//Remove the seal if it is there or not doesnt matter
	BREMOVE((_writeSealRoot + _writeSealRelative).c_str());
}

void jaspResults::finishWriting()
{
	//Let us write a small file that tells us writing stuff went well ( https://github.com/jasp-stats/INTERNAL-jasp/issues/884 )
	bofstream sealMe((_writeSealRoot + _writeSealRelative).c_str(), std::ios_base::trunc);

	sealMe << "Writing state, plot and jaspResults.json seems to have been successful!\n" << std::flush;

	sealMe.close();

	jaspPrint("Created Write Seal for jaspResults at: '" + _writeSealRoot + _writeSealRelative + "' ");
}

bool jaspResults::lastWriteWorked() const
{
	//Let us write a small file that tells us writing stuff went well ( https://github.com/jasp-stats/INTERNAL-jasp/issues/884 )
	bifstream seal((_writeSealRoot + _writeSealRelative).c_str(), std::ios_base::in);

	if(!seal.is_open()) return false;

	//std::cout << "Opened Write Seal for jaspResults to check if the last write worked from: '" << (_writeSealRoot + _writeSealRelative) << "' worked!" << std::endl;

	std::stringstream wholeSeal;

	wholeSeal << seal.rdbuf();

	seal.close();

	return wholeSeal.str().size() > 0;
}



void jaspResults::complete()
{
	jaspReport::totalWarningsClear();
	completeChildren();

	_oldResults = nullptr; //It will get destroyed in DestroyAllAllocatedObjects

	if(getStatus() == "running" || getStatus() == "waiting")
		setStatus("complete");

	saveResults();
	send();
	finishWriting();
}

void jaspResults::saveResults()
{
	JASP_OBJECT_TIMERBEGIN
	if(_saveResultsHere == "")
	{
		jaspPrint("Did not store jaspResults");
		return;
	}

	//std::cout << "Going to try to save jaspResults.json to '" << _saveResultsRoot << _saveResultsHere << "'" << std::endl;

	bofstream saveHere((_saveResultsRoot + _saveResultsHere).c_str(), std::ios_base::trunc);

	if(!saveHere.good())
	{
		static std::string error;
		error = "Could not open file for saving jaspResults! File: '" + _saveResultsRoot + _saveResultsHere + "'";
		throw std::runtime_error(error);
	}

	saveHere << convertToJSON() << std::flush;
	saveHere.close();

	// Host-specific state archive (R: RDS alongside the JSON; Python: pickle).
	// The host decides whether/what to write; see adapters.
	if(jaspHost::saveStateArchive)
		jaspHost::saveStateArchive(*this, _saveResultsRoot + _saveResultsHere);

	JASP_OBJECT_TIMEREND(saveResults)
}

void jaspResults::loadResults()
{
	JASP_OBJECT_TIMERBEGIN
	_previousOptions = Json::nullValue;

	if(_saveResultsHere == "") return;

	bifstream loadThis((_saveResultsRoot + _saveResultsHere).c_str());


	if(!loadThis.is_open()) return;

	Json::Value val;
	Json::Reader().parse(loadThis, val);

	loadThis.close();

	if(!val.isObject())
	{
		static std::string error;
		error = "loading jaspResults had a problem, '" + _saveResultsRoot + _saveResultsHere + "' wasn't a JSON object!";
		throw std::runtime_error(error);
	}

	convertFromJSON_SetFields(val);

	JASP_OBJECT_TIMEREND(loadResults);
}

void jaspResults::changeOptions(std::string opts)
{
	_previousOptions = _currentOptions;

	setOptions(opts);
}

void jaspResults::setOptions(std::string opts)
{
    // JSONCPP_STRING          err;
    // Json::CharReaderBuilder jsonReaderBuilder;
    // std::unique_ptr<Json::CharReader> const jsonReader(jsonReaderBuilder.newCharReader());

	// jsonReader->parse(opts.c_str(), opts.c_str() + opts.length(), &_currentOptions, &err);

	Json::Reader().parse(opts, _currentOptions);
	jaspObject::currentOptions = _currentOptions;

	if(_previousOptions != Json::nullValue)
		pruneInvalidatedData();
}

void jaspResults::storeOldResults()
{
	_oldResults = new jaspContainer();
	_oldResults->convertFromJSON_SetFields(jaspContainer::convertToJSON());
	_oldResults->letChildrenRun();
}

void jaspResults::pruneInvalidatedData()
{
	storeOldResults();

	checkDependenciesChildren(_currentOptions);
}

void jaspResults::send(std::string otherMsg)
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	jaspPrint("send was called!");
#endif

	if(_ipccSendFunc != nullptr)
		(*_ipccSendFunc)(otherMsg == "" ? constructResultJson() : otherMsg.c_str());
}

void jaspResults::checkForAnalysisChanged()
{
	if(_ipccPollFunc == nullptr)
		return;

	if((*_ipccPollFunc)())
	{
		jaspPrint("Polling for analysis changes found a change, analysis should restart!");
		setStatus("changed");
		if(jaspHost::signalAnalysisAbort)
			jaspHost::signalAnalysisAbort();
	}
}


void jaspResults::childrenUpdatedCallbackHandler(bool ignoreSendTimer)
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	jaspPrint("One of jaspResults children was updated!\n");
#endif

	checkForAnalysisChanged(); //can "throw" Rf_error

	if(!containsNonContainer())
		return;

	int curTime = getCurrentTimeMs();
	if(
		ignoreSendTimer													||
		_sendingFeedbackLastTime == -1									||
		(curTime - _sendingFeedbackLastTime) > _sendingFeedbackInterval
	)
	{
		send();
		_sendingFeedbackLastTime = curTime;
	}
}

Json::Value jaspResults::_response = Json::Value(Json::objectValue);

const char * jaspResults::constructResultJson()
{
	_response["typeRequest"]	= "analysis"; // Should correspond to engineState::analysis to string
	_response["results"]		= dataEntry();
	//Why was this here anyway and why is the title used as "name" because that is confusing: _response["name"]			= _response["results"]["title"];

	if(errorMessage != "" )
	{
		_response["results"]["error"]		= true;
		_response["results"]["errorMessage"] = errorMessage;
	}
	else if (_error)
	{
		_response["results"]["error"]		= true;
		_response["results"]["errorMessage"] = "Analyis returned an error but no errormessage...";
	}

	static std::string msg;
	msg = _response.toStyledString();

#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "Result JSON:\n" << msg << "\n\n" << std::flush;
#endif

	return msg.c_str();
}



Json::Value jaspResults::metaEntry() const
{
	Json::Value meta(Json::arrayValue);

	std::vector<std::string> orderedDataFields = getSortedDataFieldsWithOld(_oldResults);

	for(const std::string & field : orderedDataFields)
	{
		jaspObject *	obj			= getJaspObjectNewOrOld(field, _oldResults);
		bool			objIsOld	= jaspObjectComesFromOldResults(field, _oldResults);

		if(obj->shouldBePartOfResultsJson(true))
			meta.append(obj->metaEntry(objIsOld || !_oldResults ? nullptr : _oldResults->getJaspObjectFromData(field)));
	}

	return meta;
}

Json::Value jaspResults::dataEntry(std::string &) const
{
	Json::Value dataJson(jaspObject::dataEntryBase());

	//dataJson["title"]	= _title; We dont need this anymore. Js doesnt look at this anymore because of the solution to https://github.com/jasp-stats/jasp-issues/issues/1088. Leaving comment here for a feautre moment where someone wonders why title is a property of jaspResults but doesnt do anything.
	dataJson["name"]	= getUniqueNestedName();
	dataJson[".meta"]	= metaEntry();

	for(const std::string & field: getSortedDataFieldsWithOld(_oldResults))
	{
		jaspObject *	obj			= getJaspObjectNewOrOld(field, _oldResults);
		bool			objIsOld	= jaspObjectComesFromOldResults(field, _oldResults);
		std::string		dummyError	= "";

		if(obj->shouldBePartOfResultsJson())
			dataJson[obj->getUniqueNestedName()]	= obj->dataEntry(objIsOld || !_oldResults ? nullptr : _oldResults->getJaspObjectFromData(field), dummyError);
	}

	return dataJson;
}



void jaspResults::setErrorMessage(std::string msg, std::string errorStatus)
{
	errorMessage = msg;
	setStatus(errorStatus);
}

std::vector<jaspPlotStateEntry> jaspResults::harvestPlotObjects()
{
	std::vector<jaspPlotStateEntry> entries;

	JASP_OBJECT_TIMERBEGIN
	addPlotStateEntriesFromJaspObject(this, entries);
	JASP_OBJECT_TIMEREND(getting plot objects)

	return entries;
}

void jaspResults::addPlotStateEntriesFromJaspObject(jaspObject * obj, std::vector<jaspPlotStateEntry> & entries)
{
	if(obj->getType() == jaspObjectType::plot)
	{
		jaspPlot * plot = (jaspPlot*)obj;
		if(plot->_filePathPng != "")
			entries.push_back({ plot->_filePathPng, plot->_envName, plot->getUniqueNestedName(), plot->_width, plot->_height, plot->_revision });
	}

	for(auto c : obj->getChildren())
		addPlotStateEntriesFromJaspObject(c, entries);
}

std::vector<std::string> jaspResults::harvestStateEnvNames()
{
	std::vector<std::string> envNames;

	JASP_OBJECT_TIMERBEGIN
	addStateEnvNamesFromJaspObject(this, envNames);
	JASP_OBJECT_TIMEREND(getting other objects)

	return envNames;
}

void jaspResults::addStateEnvNamesFromJaspObject(jaspObject * obj, std::vector<std::string> & envNames)
{
	if(obj->getType() == jaspObjectType::state)
	{
		jaspState * state = (jaspState*)obj; //If other objects are needed this code can be generalized

		if(jaspHost::objectExists(state->_envName))
			envNames.push_back(state->_envName);
	}

	for(auto child : obj->getChildren())
		addStateEnvNamesFromJaspObject(child, envNames);
}

std::vector<std::string> jaspResults::harvestPlotPathsForKeep()
{
	std::vector<std::string> plotPaths;

	addPlotPathsForKeepFromJaspObject(this, plotPaths);

	return plotPaths;
}

void jaspResults::addPlotPathsForKeepFromJaspObject(jaspObject * obj, std::vector<std::string> & pngPlotPaths)
{
	if(obj->getType() == jaspObjectType::plot)
	{
		jaspPlot * plot = (jaspPlot*)obj;
		
		if(plot->_filePathPng != "")
			pngPlotPaths.push_back(plot->_filePathPng);

		if(plot->_interactiveJsonData != "")
			pngPlotPaths.push_back(plot->_interactiveJsonData);
	}

	for(auto c : obj->getChildren())
		addPlotPathsForKeepFromJaspObject(c, pngPlotPaths);
}

std::vector<std::string> jaspResults::getKeepListVector()
{
	std::vector<std::string> keep = harvestPlotPathsForKeep();

	keep.insert(keep.begin(), _saveResultsHere);
	keep.insert(keep.begin(), _writeSealRelative);
	keep.insert(keep.begin(), _relativePathKeep);

	// Also keep jaspResults.rds if it was saved alongside the JSON
	if (!_saveResultsHere.empty())
	{
		std::string rdsPath = _saveResultsHere;
		size_t dot = rdsPath.rfind('.');
		if (dot != std::string::npos)
			rdsPath.replace(dot, std::string::npos, ".rds");
		keep.insert(keep.begin(), rdsPath);
	}

	return keep;
}

Json::Value jaspResults::convertToJSON() const
{
	Json::Value obj			= jaspContainer::convertToJSON();

	obj["relativePathKeep"] = _relativePathKeep;
	obj["options"]			= _currentOptions;

	return obj;
}

void jaspResults::convertFromJSON_SetFields(Json::Value in)
{
	jaspContainer::convertFromJSON_SetFields(in);

	_relativePathKeep	= in.get("relativePathKeep",	"null").asString();
	_currentOptions		= in.get("options",				Json::objectValue);
	_previousOptions	= _currentOptions;
}



void jaspResults::startProgressbar(int expectedTicks, std::string label)
{
	_progressbarExpectedTicks		= expectedTicks;
	_progressbarLastUpdateTime		= getCurrentTimeMs();
	_progressbarTicks				= 0;

	Json::Value progress;
	progress["value"]		= 0;
	progress["label"]		= label;
	_response["progress"]	= progress;

	send();
}

void jaspResults::progressbarTick()
{
	checkForAnalysisChanged();

	_progressbarTicks++;

	int progressValue				= int(std::lround(100.0f * (float(_progressbarTicks) / float(_progressbarExpectedTicks))));
	progressValue					= std::min(100, std::max(progressValue, 0));
	_response["progress"]["value"]	= progressValue;

	int curTime = getCurrentTimeMs();
	if(curTime - _progressbarLastUpdateTime > _progressbarBetweenUpdatesTime || progressValue == 100)
	{
		send();
		_progressbarLastUpdateTime = curTime;
	}
}

//implementation here in jaspResults.cpp to make sure we have access to all constructors
jaspObject * jaspObject::convertFromJSON(Json::Value in)
{
	jaspObjectType newType = jaspObjectTypeStringToObjectType(in.get("type", "").asString());

	jaspObject * newObject = nullptr;

	switch(newType)
	{
	case jaspObjectType::qmlSource:	newObject = new jaspQmlSource();	break;
	case jaspObjectType::container:	newObject = new jaspContainer();	break;
	case jaspObjectType::table:		newObject = new jaspTable();		break;
	case jaspObjectType::plot:		newObject = new jaspPlot();			break;
	//case jaspObjectType::list:	newObject = new jaspList();			break;
	case jaspObjectType::html:		newObject = new jaspHtml();			break;
	case jaspObjectType::state:		newObject = new jaspState();		break;
	case jaspObjectType::column:	newObject = new jaspColumn();		break;
	case jaspObjectType::report:	newObject = new jaspReport();		break;
	//case jaspObjectType::results:	newObject = new jaspResults();		break;
	default:						throw std::runtime_error("Cant understand this type");
	}

	if(newObject != nullptr) newObject->convertFromJSON_SetFields(in);

	return newObject;
}
