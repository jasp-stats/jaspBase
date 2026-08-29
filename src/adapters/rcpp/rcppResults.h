#pragma once

// R-half of jaspResults (see src/core/jaspResults.h for the R-free core).
//
// Everything that used to live in the old src/jaspResults.{h,cpp} and needs R
// is here, behaviour kept identical:
//  - the R storage environment (GC-safe named store for plot/state R objects),
//  - filling it with old state objects on construction,
//  - the RDS state archive (saveStateArchive; JASP_RESULTS_RDS/JASP_RDS_STRIP),
//  - the Rcpp::List harvests (getPlotObjectsForState/getOtherObjectsForState/
//    getKeepList shapes used by common.R::finishJaspResults),
//  - XPtr registration of the send/poll function pointers,
//  - the R-facing jaspResults_Interface,
//  - the signalAnalysisAbort R call (installed on jaspHost).

#include <Rcpp.h>
#include "jaspResults.h"
#include "rcppInterfaces.h"

/// The R-side half of the old jaspResults constructor: sets up the jaspHost
/// callbacks + R storage env, fills the env with old state objects and loads
/// previous results from disk, exactly in the old order.
jaspResults *	rcppCreateJaspResults(Rcpp::String title, Rcpp::RObject oldState);

/// Old-state objects from `.retrieveState()`: figures[[$envName]] -> plotInfo
/// lists, other[[name]] -> stored objects. Moved verbatim from the old
/// jaspResults::fillEnvironmentWithStateObjects.
void			rcppFillEnvironmentWithStateObjects(Rcpp::List state);

/// XPtr unwrappers for the module registration (R: setSendFunc etc).
void			rcppSetSendFunc(Rcpp::XPtr<sendFuncDef> sendFunc);
void			rcppSetPollMessagesFunc(Rcpp::XPtr<pollMessagesFuncDef> pollFunc);

/// Keep the pre-split R-visible signatures (Rcpp::String) on the module.
void			rcppStaticStartProgressbar(int expectedTicks, Rcpp::String label);
Rcpp::String	rcppWriteSealFilename();

/// Direct access to the R storage env (used by rcppWireHostStore and the
/// old jaspResults::getObjectFromEnv/setObjectInEnv/objectExistsInEnv callers).
Rcpp::RObject	rcppGetObjectFromEnv(std::string envName);
void			rcppSetObjectInEnv(std::string envName, Rcpp::RObject obj);
bool			rcppObjectExistsInEnv(std::string envName);

/// Deletes the Rcpp wrapper of the R storage env (old ~jaspResults; wired as
/// jaspHost::destroyObjectStore).
void			rcppDestroyStorageEnv();

/// RDS state archive alongside jaspResults.json, gated on JASP_RESULTS_RDS
/// (old RDS branch of jaspResults::saveResults; wired as
/// jaspHost::saveStateArchive).
void			rcppSaveResultsAsRds(jaspResults & results, const std::string & jsonPath);

/// Rcpp::List rebuilders for today's state harvest shapes
/// (common.R::finishJaspResults depends on the exact field names).
Rcpp::List		rcppGetPlotObjectsForState(jaspResults * results);
Rcpp::List		rcppGetOtherObjectsForState(jaspResults * results);
Rcpp::List		rcppGetKeepList(jaspResults * results);

class jaspResults_Interface : public jaspContainer_Interface
{
public:
	jaspResults_Interface(jaspObject * dataObj) : jaspContainer_Interface(dataObj) {}

	void		send()								{ ((jaspResults*)myJaspObject)->send();								}
	void		complete()							{ ((jaspResults*)myJaspObject)->complete();							}
	void		saveResults()						{ ((jaspResults*)myJaspObject)->saveResults();						}
	void		finishWriting()						{ ((jaspResults*)myJaspObject)->finishWriting();					}
	Rcpp::List	getOtherObjectsForState()			{ return rcppGetOtherObjectsForState((jaspResults*)myJaspObject);	}
	Rcpp::List	getPlotObjectsForState()			{ return rcppGetPlotObjectsForState((jaspResults*)myJaspObject);	}
	Rcpp::List	getKeepList()						{ return rcppGetKeepList((jaspResults*)myJaspObject);				}
	std::string getResults()						{ return ((jaspResults*)myJaspObject)->getResults();				}

	void		setErrorMessage(Rcpp::String msg, std::string errorStatus)			{ ((jaspResults*)myJaspObject)->setErrorMessage(msg, errorStatus);							}

	void		setOptions(std::string opts)		{ ((jaspResults*)myJaspObject)->setOptions(opts); }
	void		changeOptions(std::string opts)		{ ((jaspResults*)myJaspObject)->changeOptions(opts); }

	void		setStatus(std::string status)		{ ((jaspResults*)myJaspObject)->setStatus(status); }
	std::string getStatus()							{ return ((jaspResults*)myJaspObject)->getStatus(); }

	void		prepareForWriting()					{ ((jaspResults*)myJaspObject)->prepareForWriting(); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspResults, std::string,	_relativePathKeep, RelativePathKeep)
};


RCPP_EXPOSED_CLASS_NODECL(jaspResults_Interface)
