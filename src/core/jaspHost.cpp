#include "jaspHost.h"
#include <map>

std::function<void(const std::string &)>			jaspHost::logString				= nullptr;
std::function<void(const char *)>					jaspHost::sendResults			= nullptr;
std::function<bool()>								jaspHost::pollMessages			= nullptr;
std::function<void()>								jaspHost::signalAnalysisAbort	= nullptr;
std::function<std::string(const std::string &)>		jaspHost::decodeColumnNames		= [](const std::string & str) { return str; };

std::function<jaspPlotRenderResult(const jaspPlotRenderRequest &)>	jaspHost::renderPlot		= nullptr;
std::function<void(const std::string &)>							jaspHost::saveStateArchive	= nullptr;

namespace
{
	std::map<std::string, std::any> & jaspHostObjectStore()
	{
		static std::map<std::string, std::any> store;
		return store;
	}
}

void jaspHost::storeObject(const std::string & envName, std::any obj)
{
	jaspHostObjectStore()[envName] = std::move(obj);
}

std::any jaspHost::fetchObject(const std::string & envName)
{
	auto & store = jaspHostObjectStore();
	auto it = store.find(envName);
	return it == store.end() ? std::any() : it->second;
}

bool jaspHost::objectExists(const std::string & envName)
{
	return jaspHostObjectStore().count(envName) > 0;
}

void jaspHost::clearObjects()
{
	jaspHostObjectStore().clear();
}
