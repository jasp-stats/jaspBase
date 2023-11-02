#ifndef JASPOBJECT_MANUAL_INCLUDE_GUARD
#define JASPOBJECT_MANUAL_INCLUDE_GUARD
#include <Rcpp.h>
#include <set>
#include <sstream>
#include <queue>
#include "enumutilities.h"
#include <json/json.h>

typedef void (*logFuncDef)(const std::string &);

void		setJaspLogFunction( Rcpp::XPtr<logFuncDef> func );
void		jaspPrint(			std::string msg);

#define JASPOBJECT_DEFAULT_POSITION 9999
//#define JASP_RESULTS_DEBUG_TRACES

DECLARE_ENUM(jaspObjectType, unknown, container, table, plot, list, results, html, state, column, qmlSource, report);
DECLARE_ENUM(jaspColumnType, unknown, scale, ordinal, nominal, nominalText); //can be merged with columnType from CentralDatasetModel branch later on?
DECLARE_ENUM(jaspTableColumnType, unknown, null, string, logical, integer, number, various, composite); //can be merged with columnType from CentralDatasetModel branch later on?

jaspObjectType jaspObjectTypeStringToObjectType(std::string type);

class jaspContainer;

std::string					stringExtend(std::string & str, size_t len, char kar = ' ');
std::string					stringRemove(std::string str,				char kar = ' ');
std::vector<std::string>	stringSplit(std::string str,				char kar = ';');

//Simple base-class for all JASP-objects, containing things like a title or a warning and stuff like that
class jaspObject
{
public:
						jaspObject()										: _title(""),		_type(jaspObjectType::unknown)	{ allocatedObjects->insert(this); }
						jaspObject(Rcpp::String title)						: _title(title),	_type(jaspObjectType::unknown)	{ allocatedObjects->insert(this); }
						jaspObject(jaspObjectType type, Rcpp::String title)	: _title(title),	_type(type)						{ allocatedObjects->insert(this); }
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
	virtual void		setError(Rcpp::String message)			{ _errorMessage = message; _error = true; }
	virtual bool		canShowErrorMessage()			const	{ return false; }

			void		print()									{ try { jaspPrint(toString()); } catch(std::exception e) { jaspPrint(std::string("toString failed because of: ") + e.what()); } }
			void		addMessage(std::string msg)				{ _messages.push_back(msg); }
	virtual void		childrenUpdatedCallbackHandler(bool)	{} ///Can be called by jaspResults to send changes and stuff like that.

			void		setOptionMustBeDependency(std::string optionName, Rcpp::RObject mustBeThis);
			void		setOptionMustContainDependency(std::string optionName, Rcpp::RObject mustContainThis);
			void		dependOnNestedOptions(Rcpp::CharacterVector nestedOptionName);
			void		setNestedOptionMustContainDependency(Rcpp::CharacterVector nestedOptionName, Rcpp::RObject mustContainThis);
			void		dependOnOptions(Rcpp::CharacterVector listOptions);
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

			void		childrenUpdatedCallback(bool ignoreSendTimer);
	virtual void		childFinalizedHandler(jaspObject * child) {}
			void		childFinalized(jaspObject * child);
			void		finalized();
	virtual void		finalizedHandler() {}

	virtual Rcpp::List toRObject() /*const*/ { return R_NilValue; };

	template <typename RCPP_CLASS> static std::vector<std::string> extractElementOrColumnNames(RCPP_CLASS rObj)
	{
		Rcpp::RObject colNamesRObject = Rcpp::colnames(rObj), kolnamesRObject = rObj.names();
		Rcpp::CharacterVector colNamesList;
		std::vector<std::string> colNamesVec;

		if(!colNamesRObject.isNULL() || !kolnamesRObject.isNULL())
		{
			colNamesList = !colNamesRObject.isNULL()  ? colNamesRObject : kolnamesRObject;

			for(size_t col=0; col<colNamesList.size(); col++)
				colNamesVec.push_back(Rcpp::as<std::string>(colNamesList[col]));
		}

		return colNamesVec;
	}

	static void destroyAllAllocatedObjects();

	std::set<jaspObject*> & getChildren() { return children; }

	Rcpp::DataFrame convertFactorsToCharacters(Rcpp::DataFrame df);

	static Json::Value currentOptions;

	void		notifyParentOfChanges(); ///let ancestors know about updates

	static int getCurrentTimeMs();
	static void setDeveloperMode(bool developerMode);

	bool			connectedToJaspResults();

	virtual		jaspObject *	getOldObjectFromUniqueNestedNameVector(const std::vector<std::string> &uniqueName);

	std::vector<Json::Value> RList_to_VectorJson(Rcpp::List obj);

	std::vector<Json::Value> RcppVector_to_VectorJson(Rcpp::RObject obj, bool throwError=false)
	{
		if(Rcpp::is<Rcpp::NumericVector>(obj))			return RcppVector_to_VectorJson<REALSXP>((Rcpp::NumericVector)		obj);
		else if(Rcpp::is<Rcpp::LogicalVector>(obj))		return RcppVector_to_VectorJson<LGLSXP>((Rcpp::LogicalVector)		obj);
		else if(Rcpp::is<Rcpp::IntegerVector>(obj))		return RcppVector_to_VectorJson<INTSXP>((Rcpp::IntegerVector)		obj);
		else if(Rcpp::is<Rcpp::StringVector>(obj))		return RcppVector_to_VectorJson<STRSXP>((Rcpp::StringVector)		obj);
		else if(Rcpp::is<Rcpp::CharacterVector>(obj))	return RcppVector_to_VectorJson<STRSXP>((Rcpp::CharacterVector)		obj);
		else if(isMixedRObject(obj))					return MixedRcppVector_to_VectorJson(	(Rcpp::List)				obj);
		else if(Rcpp::is<Rcpp::List>(obj))				return RList_to_VectorJson((Rcpp::List)								obj);
		else if(throwError) Rf_error("JASPjson::RcppVector_to_VectorJson received an SEXP that is not a Vector of some kind.");

		return std::vector<Json::Value>({""});
	}

	std::vector<Json::Value> MixedRcppVector_to_VectorJson(Rcpp::List obj)
	{
		std::vector<Json::Value> vec;
		Rcpp::Rcout << "MixedRcppVector_to_VectorJson" << std::endl;

		for(int i=0; i<obj.length(); i++)
		{

			Json::Value value = MixedRObject_to_JsonValue(Rcpp::as<Rcpp::List>(obj[i]));
			Rcpp::Rcout << value.toStyledString() << std::endl;

			vec.push_back(value);

//			Rcpp::List row = obj[i];
//			Json::Value value(Json::objectValue);
//			value["value"] = RObject_to_JsonValue((Rcpp::RObject)	obj[i]);

//			if (!Rcpp::is<Rcpp::CharacterVector>(obj[i][0]))
//				throw(std::runtime_error("MixedRObject type should be character but it is not!"));

//			Rcpp::CharacterVector type = obj[i+1];

//			if (type.length() != 1)
//				throw(std::runtime_error("MixedRObject type has length > 1!"));

//			value["type"] = RObject_to_JsonValue(type);

//			if (!Rcpp::is<Rcpp::CharacterVector>(obj[i+2]))
//				throw(std::runtime_error("MixedRObject format should be character but it is not!"));

//			Rcpp::CharacterVector format = obj[i+2];

//			if (format.length() != 1)
//				throw(std::runtime_error("MixedRObject format has length > 1!"));

//			value["format"] = RObject_to_JsonValue(format);

//			Rcpp::Rcout << value.toStyledString() << std::endl;

//			vec.push_back(value);

		}

		return vec;
	}

	template<int RTYPE>	 std::vector<Json::Value> RcppVector_to_VectorJson(Rcpp::Vector<RTYPE> obj)
	{
		std::vector<Json::Value> vec;

		for(int row=0; row<obj.size(); row++)
			vec.push_back(RVectorEntry_to_JsonValue(obj, row));

		return vec;
	}

	template<int RTYPE>  inline Json::Value RMatrixColumnEntry_to_JsonValue(Rcpp::MatrixColumn<RTYPE> obj, int row)	{ return ""; }

	template<int RTYPE>  inline Json::Value RVectorEntry_to_JsonValue(Rcpp::Vector<RTYPE> obj, int row)				{ return ""; }

	template<int RTYPE>	 std::vector<std::vector<Json::Value>> RcppMatrix_to_Vector2Json(Rcpp::Matrix<RTYPE>	obj)
	{
		std::vector<std::vector<Json::Value>> vecvec;

		for(int col=0; col<obj.ncol(); col++)
		{
			std::vector<Json::Value> vec;

			for(int row=0; row<obj.column(col).size(); row++)
				vec.push_back(RMatrixColumnEntry_to_JsonValue(obj.column(col), row));

			vecvec.push_back(vec);
		}

		return vecvec;
	}

	Json::Value RObject_to_JsonValue(		Rcpp::RObject	obj);
	Json::Value RObject_to_JsonValue(		Rcpp::List 		obj);
	Json::Value MixedRObject_to_JsonValue(	Rcpp::List		obj);

	bool	isMixedRObject(Rcpp::RObject obj) const { return Rcpp::is<Rcpp::List>(obj) && obj.inherits("mixed"); }

	template<int RTYPE>	 Json::Value RObject_to_JsonValue(Rcpp::Matrix<RTYPE>	obj)
	{
		Json::Value val(Json::arrayValue);

		for(int col=0; col<obj.ncol(); col++)
		{
			Json::Value valCol(Json::arrayValue);

			for(int row=0; row<obj.column(col).size(); row++)
				valCol.append(RMatrixColumnEntry_to_JsonValue(obj.column(col), row));

			val.append(valCol);
		}

		return val;
	}

	template<int RTYPE> Json::Value RObject_to_JsonValue(Rcpp::Vector<RTYPE>	obj)
	{
		Json::Value val("");

		if(obj.size() == 1)
			val = RVectorEntry_to_JsonValue(obj, 0);
		else if(obj.size() > 1)
		{
			val = Json::Value(Json::arrayValue);

			for(int row=0; row<obj.size(); row++)
				val.append(RVectorEntry_to_JsonValue(obj, row));
		}

		return val;
	}


	static Json::Value SetJson_to_ArrayJson(std::set<Json::Value> set);
	static std::set<Json::Value> ArrayJson_to_SetJson(Json::Value arr);
	static Json::Value VectorJson_to_ArrayJson(std::vector<Json::Value> vec);


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



#define TO_INFINITY_AND_BEYOND																					\
{																												\
	double val = static_cast<double>(obj[row]);																	\
	return	R_IsNA(val) ? "" :																					\
				R_IsNaN(val) ? "NaN" :																			\
					val == std::numeric_limits<double>::infinity() ? "\u221E" :									\
						val == -1 * std::numeric_limits<double>::infinity() ? "-\u221E"  :						\
							Json::Value((double)(obj[row]));													\
}

template<> inline Json::Value jaspObject::RVectorEntry_to_JsonValue<INTSXP>(Rcpp::Vector<INTSXP> obj, int row)
{
	return obj[row] == NA_INTEGER	? "" : Json::Value((int)(obj[row]));
}

template<> inline Json::Value jaspObject::RVectorEntry_to_JsonValue<LGLSXP>(Rcpp::Vector<LGLSXP> obj, int row)
{
	return obj[row] == NA_LOGICAL	? "" : Json::Value((bool)(obj[row]));
}

template<> inline Json::Value jaspObject::RVectorEntry_to_JsonValue<STRSXP>(Rcpp::Vector<STRSXP> obj, int row)
{
	return obj[row] == NA_STRING	? "" : Json::Value(_escapeHtml ? stringUtils::escapeHtmlStuff(std::string(obj[row])) : std::string(obj[row]));
}

template<> inline Json::Value jaspObject::RVectorEntry_to_JsonValue<REALSXP>(Rcpp::Vector<REALSXP> obj, int row)				TO_INFINITY_AND_BEYOND

template<> inline Json::Value jaspObject::RMatrixColumnEntry_to_JsonValue<INTSXP>(Rcpp::MatrixColumn<INTSXP> obj, int row)		{ return obj[row] == NA_INTEGER	? "" : Json::Value((int)(obj[row]));			}

template<> inline Json::Value jaspObject::RMatrixColumnEntry_to_JsonValue<LGLSXP>(Rcpp::MatrixColumn<LGLSXP> obj, int row)		{ return obj[row] == NA_LOGICAL	? "" : Json::Value((bool)(obj[row]));			}

template<> inline Json::Value jaspObject::RMatrixColumnEntry_to_JsonValue<STRSXP>(Rcpp::MatrixColumn<STRSXP> obj, int row)		{ return obj[row] == NA_STRING	? "" : Json::Value(_escapeHtml ? stringUtils::escapeHtmlStuff(std::string(obj[row])) : std::string(obj[row])); }

template<> inline Json::Value jaspObject::RMatrixColumnEntry_to_JsonValue<REALSXP>(Rcpp::MatrixColumn<REALSXP> obj, int row)	TO_INFINITY_AND_BEYOND


#define JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(JASP_TYPE, PROP_TYPE, PROP_NAME, PROP_CAPITALIZED_NAME) \
	void set ## PROP_CAPITALIZED_NAME (PROP_TYPE new ## PROP_CAPITALIZED_NAME) { ((JASP_TYPE *)myJaspObject)->PROP_NAME = new ## PROP_CAPITALIZED_NAME; myJaspObject->notifyParentOfChanges(); } \
	PROP_TYPE get ## PROP_CAPITALIZED_NAME () { return ((JASP_TYPE *)myJaspObject)->PROP_NAME; }

#define JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NO_NOTIFY(JASP_TYPE, PROP_TYPE, PROP_NAME, PROP_CAPITALIZED_NAME) \
	void set ## PROP_CAPITALIZED_NAME (PROP_TYPE new ## PROP_CAPITALIZED_NAME) { ((JASP_TYPE *)myJaspObject)->PROP_NAME = new ## PROP_CAPITALIZED_NAME; } \
	PROP_TYPE get ## PROP_CAPITALIZED_NAME () { return ((JASP_TYPE *)myJaspObject)->PROP_NAME; }

#define JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NATIVE_STRING(JASP_TYPE, PROP_NAME, PROP_CAPITALIZED_NAME) \
void set ## PROP_CAPITALIZED_NAME (Rcpp::String new ## PROP_CAPITALIZED_NAME) { ((JASP_TYPE *)myJaspObject)->PROP_NAME =  new ## PROP_CAPITALIZED_NAME; myJaspObject->notifyParentOfChanges(); } \
Rcpp::String get ## PROP_CAPITALIZED_NAME () { return ((JASP_TYPE *)myJaspObject)->PROP_NAME; }


class jaspObject_Interface
{
public:
	jaspObject_Interface(jaspObject * dataObj) : myJaspObject(dataObj)
	{
#ifdef JASP_RESULTS_DEBUG_TRACES
		std::cout << "Interface to " << dataObj->objectTitleString() << " is created!\n"<<std::flush;
#endif
	}

	jaspObject_Interface(const jaspObject_Interface* copyMe)
	{
#ifdef JASP_RESULTS_DEBUG_TRACES
		std::cout << "Interface to " << copyMe->myJaspObject->objectTitleString() << " is copied!\n"<<std::flush;
#endif
		myJaspObject = copyMe->myJaspObject;
	}

	void		print()								{ myJaspObject->print(); }
	void		addMessage(Rcpp::String msg)		{ myJaspObject->addMessage(msg); }
	std::string	toHtml()							{ return myJaspObject->toHtml(); }
	std::string	type()								{ return myJaspObject->type(); }
	void		printHtml()							{ jaspPrint(myJaspObject->toHtml()); }

	void		setOptionMustBeDependency(std::string optionName, Rcpp::RObject mustBeThis)								{ myJaspObject->setOptionMustBeDependency(optionName, mustBeThis);					}
	void		setOptionMustContainDependency(std::string optionName, Rcpp::RObject mustContainThis)					{ myJaspObject->setOptionMustContainDependency(optionName, mustContainThis);		}
	void		dependOnNestedOptions(Rcpp::CharacterVector optionName)													{ myJaspObject->dependOnNestedOptions(optionName);									}
	void		setNestedOptionMustContainDependency(Rcpp::CharacterVector optionName, Rcpp::RObject mustContainThis)	{ myJaspObject->setNestedOptionMustContainDependency(optionName, mustContainThis);	}
	void		dependOnOptions(Rcpp::CharacterVector listOptions)														{ myJaspObject->dependOnOptions(listOptions);										}
	void		copyDependenciesFromJaspObject(jaspObject_Interface * other)											{ myJaspObject->copyDependenciesFromJaspObject(other->myJaspObject);				}
	void		addCitation(Rcpp::String fullCitation)																	{ myJaspObject->addCitation(fullCitation);											}

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NATIVE_STRING(jaspObject, _title,		Title)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR_NATIVE_STRING(jaspObject, _info,		Info)
	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspObject, int,			_position,	Position)

	void		setError(Rcpp::String message)		{ myJaspObject->setError(message); }
	bool		getError()							{ return myJaspObject->getError(); }

	Rcpp::List	toRObject()							{ return myJaspObject->toRObject(); }

	jaspObject * returnMyJaspObject() { return myJaspObject; }

protected:
		jaspObject * myJaspObject = NULL;
};


void jaspObjectFinalizer(jaspObject * obj);
#define JASP_OBJECT_FINALIZER_LAMBDA(JASP_TYPE) //.finalizer( [](JASP_TYPE * obj) { std::cout << "finalizerLambda " #JASP_TYPE " Called\n" << std::flush;  jaspObjectFinalizer(obj); })

#define JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE) create_ ## JASP_TYPE
#define JASP_OBJECT_CREATOR_FUNCTIONNAME_STR(JASP_TYPE) "create_cpp_" #JASP_TYPE
#define JASP_OBJECT_CREATOR(JASP_TYPE) JASP_TYPE ## _Interface * JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE)(Rcpp::String title) { return new JASP_TYPE ## _Interface (new JASP_TYPE(title)); }
#define JASP_OBJECT_CREATOR_FUNCTIONREGISTRATION(JASP_TYPE) Rcpp::function(JASP_OBJECT_CREATOR_FUNCTIONNAME_STR(JASP_TYPE), &JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE))
#define JASP_OBJECT_CREATOR_ARG(JASP_TYPE, EXTRA_ARG) JASP_TYPE ## _Interface * JASP_OBJECT_CREATOR_FUNCTIONNAME(JASP_TYPE)(Rcpp::String title, Rcpp::RObject EXTRA_ARG) { return new JASP_TYPE ## _Interface (new JASP_TYPE(title, EXTRA_ARG)); }


RCPP_EXPOSED_CLASS_NODECL(jaspObject_Interface)

//#define JASP_R_INTERFACE_TIMERS

#ifdef JASP_R_INTERFACE_TIMERS
#define JASP_OBJECT_TIMERBEGIN			static int cumulativeTime = 0;	int startSerialize = getCurrentTimeMs();
#define JASP_OBJECT_TIMEREND(ACTIVITY)	cumulativeTime += getCurrentTimeMs() - startSerialize;	std::cout << jaspObjectTypeToString(getType()) << " spent " << cumulativeTime << "ms " #ACTIVITY "!" << std::endl;
#else
#define JASP_OBJECT_TIMERBEGIN			/* Doin' nothing */
#define JASP_OBJECT_TIMEREND(ACTIVITY)	/* What you didn't start you need not stop */
#endif

#endif
