// CORE (R-free) version of jaspState.cpp. Storage goes through the jaspHost
// object store; the R engine wires that store to jaspResults::_RStorageEnv so
// R's GC keeps the objects alive (see adapters/rcpp/rcppHost.cpp).

#include "jaspState.h"

Json::Value jaspState::convertToJSON() const
{
	Json::Value obj			= jaspObject::convertToJSON();
	obj["environmentName"]	= _envName;

	return obj;
}

void jaspState::convertFromJSON_SetFields(Json::Value in)
{
	jaspObject::convertFromJSON_SetFields(in);
	_envName = in.get("environmentName", _envName).asString();
}


void jaspState::setObject(std::any obj)
{
	jaspHost::storeObject(_envName, std::move(obj));
}

std::any jaspState::getObject()
{
	return jaspHost::fetchObject(_envName);
}

bool jaspState::hasObject() const
{
	return jaspHost::objectExists(_envName);
}

std::string jaspState::dataToString(std::string prefix) const
{
	std::stringstream out;

	out << prefix << "object stored: "	<< ( jaspHost::objectExists(_envName) ? "no" : "yes") << "\n"; // (bug-for-bug: the yes/no inversion is fixed in a later commit)

	return out.str();
}

void jaspState::initEnvName()
{
	static int counter = 0;

	_envName = "state_" + std::to_string(counter++);
}
