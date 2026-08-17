#include "jaspHost.h"
#include <map>

std::function<void(const std::string &)>			jaspHost::logString				= nullptr;
std::function<void(const char *)>					jaspHost::sendResults			= nullptr;
std::function<bool()>								jaspHost::pollMessages			= nullptr;
std::function<void()>								jaspHost::signalAnalysisAbort	= nullptr;
std::function<std::string(const std::string &)>		jaspHost::decodeColumnNames		= [](const std::string & str) { return str; };

int jaspHost::_analysisId = -1;

int jaspHost::analysisId()
{
	return _analysisId;
}

void jaspHost::setAnalysisId(int id)
{
	_analysisId = id;
}

namespace
{
	std::map<std::string, std::any> & jaspHostDefaultObjectStore()
	{
		static std::map<std::string, std::any> store;
		return store;
	}
}

std::function<void(const std::string &, std::any)>	jaspHost::storeObject	= [](const std::string & envName, std::any obj)
{
	jaspHostDefaultObjectStore()[envName] = std::move(obj);
};

std::function<std::any(const std::string &)>		jaspHost::fetchObject	= [](const std::string & envName) -> std::any
{
	auto & store = jaspHostDefaultObjectStore();
	auto it = store.find(envName);
	return it == store.end() ? std::any() : it->second;
};

std::function<bool(const std::string &)>			jaspHost::objectExists	= [](const std::string & envName)
{
	return jaspHostDefaultObjectStore().count(envName) > 0;
};

std::function<void()>								jaspHost::clearObjects	= []()
{
	jaspHostDefaultObjectStore().clear();
};

std::function<void(jaspPlot &)>						jaspHost::renderPlot		= nullptr;
std::function<void(jaspPlot &)>						jaspHost::plotStateSync		= nullptr;
std::function<void(const std::string &)>							jaspHost::saveStateArchive	= nullptr;
