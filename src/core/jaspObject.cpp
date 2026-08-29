// CORE (R-free) version of jaspObject.cpp.
// SEXP->Json conversions moved to src/adapters/rcpp/rcppConversions.cpp;
// logging/column-name decoding go through jaspHost.

#include "jaspObject.h"
#include "jaspHost.h"
#include "json/json_value.cpp" // hacky way to get libjson in the code ^^
#include "json/json_reader.cpp"
#include "json/json_writer.cpp"

#include <iostream>
#include <chrono>
#include <cctype>
#include <stdexcept>


std::string stringExtend(std::string & str, size_t len, char kar)
{
	std::string uit(str);

	while(uit.size() < len)
		uit += kar;

	return uit;
}

std::string stringRemove(std::string str, char kar)
{
	std::string uit;

	for(char k : str)
		if(k != kar)
			uit.push_back(k);

	return uit;
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

void jaspPrint(std::string msg)
{
	msg = decodeColumnNames(msg);

	if(jaspHost::logString)
		jaspHost::logString(msg + "\n");
	else
		std::cout << msg << "\n";
}

std::string decodeColumnNames(const std::string & str)
{
	if(jaspHost::decodeColumnNames)
		return jaspHost::decodeColumnNames(str);

	return str;
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

void jaspObject::dependOnOptions(std::vector<std::string> listOptions)
{
	if(currentOptions.isNull()) throw std::runtime_error("No options known!");

	for(auto & name : listOptions)
	{
		std::string nameTypes = name + ".types";
		_optionMustBe[name] = currentOptions.get(name, Json::nullValue);
		if (currentOptions.isMember(nameTypes))
			_optionMustBe[nameTypes] = currentOptions.get(nameTypes, Json::nullValue);
	}
}

void jaspObject::setOptionMustBeDependency(std::string optionName, Json::Value mustBeThis)
{
	_optionMustBe[optionName]	= mustBeThis;
}

void jaspObject::setOptionMustContainDependency(std::string optionName, Json::Value mustContainThis)
{
	if (mustContainThis.isNull())
		throw std::runtime_error("setOptionMustContainDependency expected not null!");

	_optionMustContain[optionName] = mustContainThis;
}

void jaspObject::dependOnNestedOptions(std::vector<std::string> nestedKey)
{
	Json::Value obj = getObjectFromNestedOption(nestedKey);
	if (obj.isNull())
		throw std::runtime_error("nested key \"" + nestedKeyToString(nestedKey, "$") + "\" does not exist in the options!");

	_nestedOptionMustBe[nestedKey] = obj;
}

void jaspObject::setNestedOptionMustContainDependency(std::vector<std::string> nestedOptionName, Json::Value mustContainThis)
{
	if (mustContainThis.isNull())
		throw std::runtime_error("setNestedOptionMustContainDependency expected not null!");

	std::vector<std::string> nestedKey = nestedOptionName;
	Json::Value obj = getObjectFromNestedOption(nestedKey);
	if (obj.isNull())
		throw std::runtime_error("nested key \"" + nestedKeyToString(nestedKey, "$") + "\" does not exist in the options!");

	_nestedOptionMustContain[nestedKey] = mustContainThis;
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
