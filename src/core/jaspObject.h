#ifndef JASPOBJECT_MANUAL_INCLUDE_GUARD
#define JASPOBJECT_MANUAL_INCLUDE_GUARD

// CORE (R-free) version of jaspObject.h.
// The SEXP->Json conversion helpers moved to src/adapters/rcpp/rcppConversions.h,
// the R-facing jaspObject_Interface + module macros to
// src/adapters/rcpp/jaspObjectInterface.h, toRObject() dispatch to
// src/adapters/rcpp/rcppToRObject.*. Logging/decoding go through jaspHost.

#include <set>
#include <sstream>
#include <queue>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include "jaspEnums.h"
#include <json/json.h>

typedef void (*logFuncDef)(const std::string &);

void		jaspPrint(			std::string msg);
std::string	decodeColumnNames(const	std::string & str);


#define JASPOBJECT_DEFAULT_POSITION 9999
//#define JASP_RESULTS_DEBUG_TRACES

class jaspContainer;

std::string					stringExtend(std::string & str, size_t len, char kar = ' ');
std::string					stringRemove(std::string str,				char kar = ' ');
std::vector<std::string>	stringSplit(std::string str,				char kar = ';');

//Simple base-class for all JASP-objects, containing things like a title or a warning and stuff like that
class jaspObject
{
public:
						jaspObject()										: _title(""),		_type(jaspObjectType::unknown)	{ allocatedObjects->insert(this); }
						jaspObject(std::string title)						: _title(title),	_type(jaspObjectType::unknown)	{ allocatedObjects->insert(this); }
						jaspObject(jaspObjectType type, std::string title)	: _title(title),	_type(type)						{ allocatedObjects->insert(this); }
						jaspObject(const jaspObject& that) = delete;
	virtual				~jaspObject();

			std::string objectTitleString(std::string prefix="")	const { return prefix + jaspObjectTypeToString(_type) + " " + _title; }
	virtual	std::string dataToString(std::string)					const { return ""; }
			std::string toString(std::string prefix = "")			const;

	virtual std::string toHtml()	const { return ""; }
			std::string htmlTitle() const { return "<h2>" + _title + "</h2>"; }

			std::string type() { return jaspObjectTypeToString(_type); }

			bool		getError()								{ return _error; }
	virtual void		setError()								{ _error = true; }
	virtual void		setError(std::string message)			{ _errorMessage = message; _error = true; }
	virtual void		clearError()							{ _error = false; _errorMessage.clear(); }
	virtual bool		canShowErrorMessage()			const	{ return false; }

			void		print()									{ try { jaspPrint(toString()); } catch(std::exception e) { jaspPrint(std::string("toString failed because of: ") + e.what()); } }
			void		addMessage(std::string msg)				{ _messages.push_back(msg); }
	virtual void		childrenUpdatedCallbackHandler(bool)	{} ///Can be called by jaspResults to send changes and stuff like that.

			void		setOptionMustBeDependency(std::string optionName, Json::Value mustBeThis);
			void		setOptionMustContainDependency(std::string optionName, Json::Value mustContainThis);
			void		dependOnNestedOptions(std::vector<std::string> nestedOptionName);
			void		setNestedOptionMustContainDependency(std::vector<std::string> nestedOptionName, Json::Value mustContainThis);
			void		dependOnOptions(std::vector<std::string> listOptions);
			void		copyDependenciesFromJaspObject(jaspObject * other);

			bool		checkDependencies(Json::Value currentOptions); //returns false if no longer valid and destroys children (if applicable) that are no longer valid
	virtual	void		checkDependenciesChildren(Json::Value currentOptions) {}

			void		addCitation(std::string fullCitation);

			std::string	_title,
						_info;
			int			_position = JASPOBJECT_DEFAULT_POSITION;

			jaspObjectType	getType()															const { return _type; }
	virtual bool			shouldBePartOfResultsJson(bool meta = false)						const { return _type != jaspObjectType::state; }

			Json::Value		constructMetaEntry(std::string type, std::string meta = "")			const;

	//These functions convert the object to a json that can be understood by the resultsviewer
	virtual	Json::Value		metaEntry()															const { return Json::Value(Json::nullValue); }
	virtual	Json::Value		dataEntry(std::string & errorMessage)								const ;

	//These two are meant for jaspContainer and take old results into account and a possible errorMessage
	virtual	Json::Value		metaEntry(jaspObject * oldResult)									const { return metaEntry(); }
	virtual	Json::Value		dataEntry(jaspObject * oldResult, std::string & errorMessage)		const { return dataEntry(errorMessage); }

			Json::Value		dataEntryBase()														const;

	//These functions convert to object and all to a storable json-representation that can be written to disk and loaded again.
	virtual Json::Value		convertToJSON() const;
	static	jaspObject *	convertFromJSON(Json::Value in);
	virtual	void			convertFromJSON_SetFields(Json::Value in);

			///Gives nested name to avoid namingclashes
			std::string getUniqueNestedName() const;
			void		getUniqueNestedNameVector(std::vector<std::string> & names)	const;
			void		setName(std::string name) { _name = name; }
			const std::string & name() const { return _name; }

			void		childrenUpdatedCallback(bool ignoreSendTimer);
	virtual void		childFinalizedHandler(jaspObject * child) {}
			void		childFinalized(jaspObject * child);
			void		finalized();
	virtual void		finalizedHandler() {}

	static void destroyAllAllocatedObjects();

	std::set<jaspObject*> & getChildren() { return children; }

	static Json::Value currentOptions;

	void		notifyParentOfChanges(); ///let ancestors know about updates

	static int getCurrentTimeMs();
	static void setDeveloperMode(bool developerMode);

	bool			connectedToJaspResults();

	virtual		jaspObject *	getOldObjectFromUniqueNestedNameVector(const std::vector<std::string> &uniqueName);

	static Json::Value SetJson_to_ArrayJson(std::set<Json::Value> set);
	static std::set<Json::Value> ArrayJson_to_SetJson(Json::Value arr);
	static Json::Value VectorJson_to_ArrayJson(std::vector<Json::Value> vec);

	bool		getEscapeHtml() const { return _escapeHtml; }

protected:
	jaspObjectType				_type;
	std::string					_errorMessage = "";
	bool						_error = false,
								_escapeHtml = true; // Used to escape Html characters when converting R object to Json. This is true per default, because the results of these objects are usually send to a Web Browser.

	std::vector<std::string>	_messages;
	std::set<std::string>		_citations;
	std::string					_name;

	std::set<std::string>								nestedMustBes()			const;
	std::map<std::string, std::set<std::string>>		nestedMustContains()	const;
	std::map<std::string, Json::Value>					_optionMustContain;
	std::map<std::string, Json::Value>					_optionMustBe;
	std::map<std::vector<std::string>, Json::Value>		_nestedOptionMustContain;
	std::map<std::vector<std::string>, Json::Value>		_nestedOptionMustBe;


//Should add dependencies somehow here?

//Some basic administration of objecttree:
			bool			hasAncestor(jaspObject * ancestor) { return parent == ancestor || parent == NULL ? false : parent->hasAncestor(ancestor); }
			void			addChild(jaspObject * child);

			void			removeChild(jaspObject * child);


	jaspObject				*parent = NULL;
	std::set<jaspObject*>	children;

	static std::set<jaspObject*> *	allocatedObjects;
	static bool						_developerMode;

private:

	Json::Value					getObjectFromNestedOption(std::vector<std::string> nestedKey, Json::Value ifNotFound = Json::nullValue) const;
	std::string					nestedKeyToString(const std::vector<std::string> & nestedKey, const std::string & sep = "$!_SEP_!$")	const;
	std::vector<std::string>	stringToNestedKey(const std::string & nestedKey, const std::string & sep = "$!_SEP_!$")					const;
	bool						isJsonSubArray(const Json::Value needle, const Json::Value haystack)									const;

	bool					_finalizedAlready = false;
};

void jaspObjectFinalizer(jaspObject * obj);

//#define JASP_R_INTERFACE_TIMERS

#ifdef JASP_R_INTERFACE_TIMERS
#define JASP_OBJECT_TIMERBEGIN			static int cumulativeTime = 0;	int startSerialize = getCurrentTimeMs();
#define JASP_OBJECT_TIMEREND(ACTIVITY)	cumulativeTime += getCurrentTimeMs() - startSerialize;	std::cout << jaspObjectTypeToString(getType()) << " spent " << cumulativeTime << "ms " #ACTIVITY "!" << std::endl;
#else
#define JASP_OBJECT_TIMERBEGIN			/* Doin' nothing */
#define JASP_OBJECT_TIMEREND(ACTIVITY)	/* What you didn't start you need not stop */
#endif

#endif
