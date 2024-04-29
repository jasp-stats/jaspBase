#define ENUM_DECLARATION_CPP
#include "jaspObject.h"
#include <chrono>

#ifdef BUILDING_JASP
#include <json/json.h>
#else
#include "json/json_value.cpp" // hacky way to get libjson in the code ^^
#include "json/json_reader.cpp"
#include "json/json_writer.cpp"
#endif

jaspObjectType jaspObjectTypeStringToObjectType(std::string type)
{
	try			{ return jaspObjectTypeFromString(type); }
	catch(...)	{ return jaspObjectType::unknown; }
}

std::string stringExtend(std::string & str, size_t len, char kar)
{
	if(str.size() < len)
		str += std::string(len - str.size(), kar);

	return str;
}

std::string stringRemove(std::string str, char kar)
{
	for(size_t removeMe = str.find_first_of(kar); removeMe != std::string::npos; removeMe = str.find_first_of(kar))
		str.erase(removeMe, 1);
	return str;
}

std::vector<std::string> stringSplit(std::string str, char kar)
{
	std::vector<std::string> strs;

	strs.push_back("");
	for(char k : str)
		if(k == kar)
			strs.push_back("");
		else
			strs[strs.size() - 1].push_back(k);

	return strs;
}

logFuncDef _jaspRCPP_logString = nullptr;

void		setJaspLogFunction(Rcpp::XPtr<logFuncDef> func)
{
	_jaspRCPP_logString = * func;

	_jaspRCPP_logString("Log string function received loud and clear!");
}

void jaspPrint(std::string msg)
{
#ifdef JASP_R_INTERFACE_LIBRARY
	_jaspRCPP_logString(msg + "\n");
#else
	Rcpp::Rcout << msg << "\n";
#endif
}

std::set<jaspObject*> * jaspObject::allocatedObjects = new std::set<jaspObject*>();

jaspObject::~jaspObject()
{
	allocatedObjects->erase(this);

	if(parent != NULL)
		parent->removeChild(this);

	while (children.size() > 0)
	{
		jaspObject * p = *(children.begin());

		removeChild(p);

		delete p;
	}
}

void jaspObject::destroyAllAllocatedObjects()
{
	//std::cout << "destroyAllAllocatedObjects!\n"<<std::flush;
	while(allocatedObjects->size() > 0)
	{
		jaspObject * p = *(allocatedObjects->begin());

		//std::cout << "p == "<<p->objectTitleString()<<"!\n"<<std::flush;

		allocatedObjects->erase(allocatedObjects->begin());
		delete p;
	}
}

void jaspObject::addChild(jaspObject * child)
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << _title << " adds Child " << child->_title << "\n" << std::flush;
#endif

	if(child->parent == this)
		return;

	if(child == this || hasAncestor(child))
		throw std::logic_error("You cannot make someone their own descendant, this isn't back to the future..");

	if(child->parent != NULL)
		child->parent->children.erase(child);

	child->parent = this;

	children.insert(child);
}

void jaspObject::removeChild(jaspObject * child)
{
	if(child->parent != this || child == NULL)
		return;

	children.erase(child);

	child->parent = NULL;
}

Json::Value jaspObject::getObjectFromNestedOption(std::vector<std::string> nestedKey, Json::Value ifNotFound) const
{
	Json::Value obj = currentOptions;
	for (const auto& key: nestedKey)
	{
		// NOTE: this fails if we have options which are an array where some elements are named, but not all.
		// I think that would violate the Json spec for arrays, but I'm not 100% sure.
		if (obj.isArray() && std::all_of(key.begin(), key.end(), ::isdigit))
		{
			int index = stoi(key) - 1; // So R users can use 1-based indexing
			obj = obj.get(index, Json::nullValue);
		}
		else
			obj = obj.get(key, Json::nullValue);

		if (obj.isNull())
			return ifNotFound;

	}
	return obj;
}

std::string jaspObject::nestedKeyToString(const std::vector<std::string> &nestedKey, const std::string &sep) const
{

	std::string joined;

	for (int i = 0; i < nestedKey.size() - 1; i++)
		joined += nestedKey[i] + sep;

	joined += nestedKey[nestedKey.size() - 1];

	return joined;

}

std::vector<std::string> jaspObject::stringToNestedKey(const std::string &str, const std::string &sep) const
{

	std::vector<std::string> nestedKey;
	size_t noKeys = 1;

	std::string::size_type pos = 0;
	while ((pos = str.find(sep, pos)) != std::string::npos)
	{
		noKeys++;
		pos += sep.length();
	}

	nestedKey.reserve(noKeys);
	pos = 0;
	std::string s = str; // explicit copy to avoid modifying str
	// Could also be done with 2 positions, e.g., s.substr(start, stop);
	while ((pos = s.find(sep)) != std::string::npos)
	{
		nestedKey.push_back(s.substr(0, pos));
		s.erase(0, pos + sep.length());
	}
	nestedKey.push_back(s);

	return nestedKey;
}

bool jaspObject::isJsonSubArray(const Json::Value needles, const Json::Value haystack) const
{
	// all(needles %in% haystack) in R.

	if (haystack.empty())
		return false;

	if (needles == haystack)
		return true;

	if (!(needles.isArray() || haystack.isArray()) || (needles.isArray() && !haystack.isArray()))
		return false;

	if (needles.isArray())
	{
		for (const auto & needle: needles)
		{
			bool foundIt = false;
			for (const auto & hay : haystack)
				if (needle == hay)
				{
					foundIt = true;
					break;
				}

			if (!foundIt)
				return false;
		}
	}
	else // haystack must be an array and needles a single value
	{
		bool foundIt = false;
		for (const auto & hay : haystack)
			if (needles == hay)
			{
				foundIt = true;
				break;
			}

		if (!foundIt)
			return false;
	}

	return true;
}

void jaspObject::finalized()
{
	//std::cout << "jaspObject::finalized() called on "<<objectTitleString()<<" " << (_finalizedAlready ? "again!" :"") << "\n" << std::flush;
	//std::cout << "this: "<<this<<"\n"<<std::flush;

	if(_finalizedAlready)
		return;

	_finalizedAlready= true;

	if(parent != NULL)
		parent->childFinalized(this);

	finalizedHandler();

	for(auto child : children)
		child->finalized();
}

void jaspObject::childFinalized(jaspObject * child)
{
	finalized();

	childFinalizedHandler(child);
	removeChild(child);
}

void jaspObject::notifyParentOfChanges()
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "notifyParentOfChanges()! parent is " << ( parent == NULL ? "NULL" : parent->_title) << "\n" << std::flush;
#endif

	if(parent != NULL)
		parent->childrenUpdatedCallback(false);
}

void jaspObject::childrenUpdatedCallback(bool ignoreSendTimer)
{
#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "childrenUpdatedCallback()! parent is " << ( parent == NULL ? "NULL" : parent->_title) << "\n" << std::flush;
#endif

	childrenUpdatedCallbackHandler(ignoreSendTimer);

	if(parent != NULL)
		parent->childrenUpdatedCallback(ignoreSendTimer);
}

std::string jaspObject::toString(std::string prefix) const
{
	std::string dataString = dataToString(prefix + "\t");
	return objectTitleString(prefix) + (dataString == "" ? "\n" : ":\n" + dataString);
}

Rcpp::DataFrame jaspObject::convertFactorsToCharacters(Rcpp::DataFrame df)
{

	for(int col=0; col<df.length(); col++)
		if(Rf_isFactor(df[col]))
		{
			Rcpp::IntegerVector		originalColumn	= df[col];

			Rcpp::CharacterVector	factorLevels	= originalColumn.attr("levels");

/*#ifdef JASP_DEBUG
			//In ifdef because we dont really have access to log here.
			std::cout	<< "converting factors to characters for dataframe\n"
						<< "originalColumn: " << originalColumn << "\n"
						<< "factorLevels: " << factorLevels << std::endl;
#endif*/

			Rcpp::CharacterVector	charCol(originalColumn.size());

			for(int i=0; i<originalColumn.size(); i++)
				if(originalColumn[i] > 0) //it can be INT_MIN at least, but if we are doing a -1 on it anyhow it should just be bigger than 0
					charCol[i] = factorLevels[originalColumn[i] - 1];

			df[col] = charCol;
		}

	return df;
}

Json::Value	jaspObject::constructMetaEntry(std::string type, std::string meta) const
{
	Json::Value obj(Json::objectValue);

	obj["name"]  = getUniqueNestedName();
	obj["type"]  = type;
	obj["info"]  = _info;
	obj["title"] = _title;

	if(meta != "")
		obj["meta"] = meta;

	if(_developerMode)
	{
		obj["mustBe"]		= Json::arrayValue;
		for(const std::string & mustBe : nestedMustBes())
			obj["mustBe"].append(mustBe);

		obj["mustContain"]	= Json::objectValue;
		for(const auto & keyval : nestedMustContains())
		{
			obj["mustContain"][keyval.first] = Json::arrayValue;

			for(const std::string & containThis : keyval.second)
				obj["mustContain"][keyval.first].append(containThis);
		}

		// TODO: should we add the nestedOptions*** here too?

	}

	return obj;
}

std::string jaspObject::getUniqueNestedName() const
{
	std::string parent_prefix = parent == NULL || parent->getUniqueNestedName() == "" ? "" :  parent->getUniqueNestedName() + "_";

	return parent_prefix + (_name != "" ? _name : "");
}

void jaspObject::getUniqueNestedNameVector(std::vector<std::string> &names) const
{
	if (parent)
		parent->getUniqueNestedNameVector(names);

	// jaspResults doesn't have a name
	if (_name != "")
		names.push_back(_name);

}


void jaspObjectFinalizer(jaspObject * obj)
{
	if(obj == NULL)
		return;

#ifdef JASP_RESULTS_DEBUG_TRACES
	std::cout << "JASPobjectFinalizer is run on: " << obj->_title << "\n" << std::flush;
#endif

	obj->finalized();
}

Json::Value jaspObject::convertToJSON() const
{
	Json::Value obj(Json::objectValue);

	obj["name"]			= _name;
	obj["title"]		= _title;
	obj["type"]			= jaspObjectTypeToString(_type);
	obj["error"]        = _error;
	obj["errorMessage"] = _errorMessage;
	obj["position"]		= _position;
	obj["escapeHtml"]	= _escapeHtml;
	obj["citations"]	= Json::arrayValue;
	obj["messages"]		= Json::arrayValue;

	for(auto c : _citations)
		obj["citations"].append(c);

	for(auto m : _messages)
		obj["messages"].append(m);

	obj["optionMustBe"]	= Json::objectValue;
	for(auto & keyval : _optionMustBe)
		obj["optionMustBe"][keyval.first] = keyval.second;

	obj["optionMustContain"]	= Json::objectValue;
	for(auto & keyval : _optionMustContain)
		obj["optionMustContain"][keyval.first] = keyval.second;

	obj["nestedOptionMustBe"]	= Json::objectValue;
	for(auto & keyval : _nestedOptionMustBe)
		obj["nestedOptionMustBe"][nestedKeyToString(keyval.first)] = keyval.second;

	obj["nestedOptionMustContain"]	= Json::objectValue;
	for(auto & keyval : _nestedOptionMustContain)
		obj["nestedOptionMustContain"][nestedKeyToString(keyval.first)] = keyval.second;


	return obj;
}


void jaspObject::convertFromJSON_SetFields(Json::Value in)
{
	_name			= in.get("name",			"null").asString();
	_title			= in.get("title",			"null").asString();
	_error			= in.get("error",			false).asBool();
	_errorMessage	= in.get("errorMessage",	"").asString();
	_position		= in.get("position",		JASPOBJECT_DEFAULT_POSITION).asInt();
	_escapeHtml		= in.get("escapeHtml",		true).asBool();

	_citations.clear();
	for(auto & citation : in.get("citations", Json::nullValue))
		_citations.insert(citation.asString());

	_messages.clear();
	for(auto & msg : in.get("messages", Json::nullValue))
		_messages.push_back(msg.asString());

	_optionMustBe.clear();
	Json::Value mustBe(in.get("optionMustBe", Json::objectValue));
	for(auto & mustBeKey : mustBe.getMemberNames())
		_optionMustBe[mustBeKey] = mustBe[mustBeKey];

	_optionMustContain.clear();
	Json::Value mustContain(in.get("optionMustContain", Json::objectValue));
	for(auto & mustContainKey : mustContain.getMemberNames())
		_optionMustContain[mustContainKey] = mustContain[mustContainKey];

	_nestedOptionMustBe.clear();
	Json::Value nestedMustBe(in.get("nestedOptionMustBe", Json::objectValue));
	for(auto & nestedMustBeKey : nestedMustBe.getMemberNames())
		_nestedOptionMustBe[stringToNestedKey(nestedMustBeKey)] = nestedMustBe[nestedMustBeKey];

	_nestedOptionMustContain.clear();
	Json::Value nestedMustContain(in.get("nestedOptionMustContain", Json::objectValue));
	for(auto & nestedMustContainKey : nestedMustContain.getMemberNames())
		_nestedOptionMustContain[stringToNestedKey(nestedMustContainKey)] = nestedMustContain[nestedMustContainKey];

}

Json::Value jaspObject::currentOptions = Json::nullValue;

void jaspObject::dependOnOptions(Rcpp::CharacterVector listOptions)
{
	if(currentOptions.isNull()) Rf_error("No options known!");

	for(auto & nameOption : listOptions)
		_optionMustBe[Rcpp::as<std::string>(nameOption)] = currentOptions.get(nameOption, Json::nullValue);
}

void jaspObject::setOptionMustBeDependency(std::string optionName, Rcpp::RObject mustBeThis)
{
	_optionMustBe[optionName]	= RObject_to_JsonValue(mustBeThis);
}

void jaspObject::setOptionMustContainDependency(std::string optionName, Rcpp::RObject mustContainThis)
{
	if (mustContainThis.isNULL())
		Rf_error("setOptionMustContainDependency expected not null!");

	_optionMustContain[optionName] = RObject_to_JsonValue(mustContainThis);
}

void jaspObject::dependOnNestedOptions(Rcpp::CharacterVector nestedOptionName)
{
	std::vector<std::string> nestedKey = Rcpp::as<std::vector<std::string>>(nestedOptionName);
	Json::Value obj = getObjectFromNestedOption(nestedKey);
	if (obj.isNull())
		Rf_error("nested key `%s` does not exist in the options!", nestedKeyToString(nestedKey, "$").c_str());

	_nestedOptionMustBe[nestedKey] = obj;
}

void jaspObject::setNestedOptionMustContainDependency(Rcpp::CharacterVector nestedOptionName, Rcpp::RObject mustContainThis)
{
	if (mustContainThis.isNULL())
		Rf_error("setNestedOptionMustContainDependency expected not null!");

	std::vector<std::string> nestedKey = Rcpp::as<std::vector<std::string>>(nestedOptionName);
	Json::Value obj = getObjectFromNestedOption(nestedKey);
	if (obj.isNull())
		Rf_error("nested key `%s` does not exist in the options!", nestedKeyToString(nestedKey, "$").c_str());

	_nestedOptionMustContain[nestedKey] = RObject_to_JsonValue(mustContainThis);
}


void jaspObject::copyDependenciesFromJaspObject(jaspObject * other)
{
	for(auto fieldVal : other->_optionMustBe)
		_optionMustBe[fieldVal.first] = fieldVal.second;

	for(auto fieldVal : other->_optionMustContain)
		_optionMustContain[fieldVal.first] = fieldVal.second;

	for(auto fieldVal : other->_nestedOptionMustBe)
		_nestedOptionMustBe[fieldVal.first] = fieldVal.second;

	for(auto fieldVal : other->_nestedOptionMustContain)
		_nestedOptionMustContain[fieldVal.first] = fieldVal.second;
}

bool jaspObject::checkDependencies(Json::Value currentOptions)
{
	if((_optionMustBe.size() + _optionMustContain.size() + _nestedOptionMustBe.size() + _nestedOptionMustContain.size()) != 0)
	{

		for(auto & keyval : _optionMustBe)
			if(currentOptions.get(keyval.first, Json::nullValue) != keyval.second)
				return false;

		for(auto & keyval : _optionMustContain)
			if (!isJsonSubArray(keyval.second, currentOptions.get(keyval.first, Json::arrayValue)))
				return false;

		for(auto & keyval : _nestedOptionMustBe)
			if(getObjectFromNestedOption(keyval.first) != keyval.second)
				return false;

		for(auto & keyval : _nestedOptionMustContain)
			if (!isJsonSubArray(keyval.second, getObjectFromNestedOption(keyval.first, Json::arrayValue)))
				return false;

	}

	checkDependenciesChildren(currentOptions);

	return true;
}

void jaspObject::addCitation(std::string fullCitation)
{
	bool citationAdded = _citations.insert(fullCitation).second;
	if (citationAdded)
		notifyParentOfChanges();
}

Json::Value	jaspObject::dataEntry(std::string & errorMessage) const
{
	Json::Value baseObject(dataEntryBase());

	//cascaded errorMessage supersedes _errorMessage
	if(canShowErrorMessage() && (errorMessage != "" || _errorMessage != "" || _error))
	{
		baseObject["error"]					= Json::objectValue;
		baseObject["error"]["type"]			= "badData"; // I guess?
		baseObject["error"]["errorMessage"] = errorMessage != "" ? errorMessage : _errorMessage; //I guess the errormessage will be blank if only _error is set somehow?

		errorMessage						= ""; //because this is a reference this will make sure it will not be added to the next child
	}

	return baseObject;
}

Json::Value	jaspObject::dataEntryBase() const
{
	Json::Value baseObject(Json::objectValue);
	for(auto c : _citations)
		baseObject["citation"].append(c);

	return baseObject;
}

int jaspObject::getCurrentTimeMs()
{
	return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
}

bool jaspObject::_developerMode	= false;

void jaspObject::setDeveloperMode(bool developerMode)
{
	_developerMode = developerMode;
}

bool jaspObject::connectedToJaspResults()
{

	if (getType() == jaspObjectType::results)
		return true;

	if (parent == nullptr)
		return false;

	return parent->connectedToJaspResults();

}

jaspObject *jaspObject::getOldObjectFromUniqueNestedNameVector(const std::vector<std::string> &uniqueName)
{
	return parent != nullptr ? parent->getOldObjectFromUniqueNestedNameVector(uniqueName) : nullptr;
}

std::set<std::string> jaspObject::nestedMustBes() const
{
	std::set<std::string> out = parent ? parent->nestedMustBes() : std::set<std::string>({});

	for(const auto & keyval : _optionMustBe)
		out.insert(keyval.first);

	return out;
}

std::map<std::string, std::set<std::string>> jaspObject::nestedMustContains() const
{
	std::map<std::string, std::set<std::string>> out = parent ? parent->nestedMustContains() : std::map<std::string, std::set<std::string>>({});

	for(const auto & keyval : _optionMustContain)
		if(keyval.second.isArray())
			for(const Json::Value & entry : keyval.second)
				out[keyval.first].insert(entry.asString());
		else if(keyval.second.isString())
			out[keyval.first].insert(keyval.second.asString());
		else
			jaspPrint("Trying to get nestedMustContains for jaspObject '" + toString() + "' but it isn't an array of strings or a string...");

	return out;
}

std::vector<Json::Value> jaspObject::RList_to_VectorJson(Rcpp::List obj)
{
	std::vector<Json::Value> vec;

	for(int row=0; row<obj.size(); row++)
		vec.push_back(RObject_to_JsonValue((Rcpp::RObject)obj[row]));

	return vec;
}

Json::Value jaspObject::RObject_to_JsonValue(Rcpp::RObject obj)
{
	if(obj.isNULL())								return Json::nullValue;
	else if(Rcpp::is<Rcpp::List>(obj))				return RObject_to_JsonValue((Rcpp::List)					obj);
	else if(Rcpp::is<Rcpp::DataFrame>(obj))			return RObject_to_JsonValue((Rcpp::List)					obj);
	else if(Rcpp::is<Rcpp::NumericMatrix>(obj))		return RObject_to_JsonValue<REALSXP>((Rcpp::NumericMatrix)	obj);
	else if(Rcpp::is<Rcpp::NumericVector>(obj))		return RObject_to_JsonValue<REALSXP>((Rcpp::NumericVector)	obj);
	else if(Rcpp::is<Rcpp::IntegerVector>(obj))		return RObject_to_JsonValue<INTSXP>((Rcpp::IntegerVector)	obj);
	else if(Rcpp::is<Rcpp::LogicalVector>(obj))		return RObject_to_JsonValue<LGLSXP>((Rcpp::LogicalVector)	obj);
	else if(Rcpp::is<Rcpp::CharacterVector>(obj))	return RObject_to_JsonValue<STRSXP>((Rcpp::CharacterVector)	obj);
	else if(Rcpp::is<Rcpp::StringVector>(obj))		return RObject_to_JsonValue<STRSXP>((Rcpp::StringVector)	obj);
	else if(obj.isS4())								return "an S4, which is too complicated for jaspResults now.";
	else											return "something that is not understood by jaspResults right now..";
}

Json::Value jaspObject::RObject_to_JsonValue(Rcpp::List obj)
{
	bool atLeastOneNamed = false;

	Rcpp::RObject namesListRObject = obj.names();
	Rcpp::CharacterVector namesList;

	if(!namesListRObject.isNULL())
	{
		namesList = namesListRObject;

		for(int row=0; row<obj.size(); row++)
			if(namesList[row] != "")
				atLeastOneNamed = true;
	}

	Json::Value val = atLeastOneNamed ? Json::objectValue : Json::arrayValue;

	if(atLeastOneNamed)
		for(int row=obj.size() - 1; row>=0; row--) //We go backwards because in R the first entry of a name in a list is used. So to emulate this we go backwars and we override an earlier occurence. (aka you have two elements with the name "a" in a list and in R list$a returns the first occurence. This is now also the element visible in the json.)
		{
			std::string name(namesList[row]);

			if(name == "")
				name = "element_" + std::to_string(row);

			val[name] = RObject_to_JsonValue((Rcpp::RObject)obj[row]);
		}
	else
		for(int row=0; row<obj.size(); row++)
			val.append(RObject_to_JsonValue((Rcpp::RObject)obj[row]));


	return val;
}

Json::Value jaspObject::SetJson_to_ArrayJson(std::set<Json::Value> set)
{
	Json::Value array(Json::arrayValue);
	for(auto val: set)
		array.append(val);
	return array;
}

std::set<Json::Value> jaspObject::ArrayJson_to_SetJson(Json::Value arr)
{
	std::set<Json::Value> set;
	for(auto & val: arr)
		set.insert(val);
	return set;
}

Json::Value jaspObject::VectorJson_to_ArrayJson(std::vector<Json::Value> vec)
{
	Json::Value array(Json::arrayValue);
	for(auto val: vec)
		array.append(val);
	return array;
}


