#pragma once

// CORE (R-free) version of jaspContainer.h. The Rcpp-facing insert dispatch,
// wrapJaspObject, list-construction and toRObject live in
// src/adapters/rcpp/rcppContainer; jaspContainer_Interface lives in
// src/adapters/rcpp/rcppInterfaces.h. Core children only: insert(jaspObject*)
// and at() -> jaspObject*.

#include "jaspObject.h"
#include <map>

class jaspContainer : public jaspObject
{
public:
	jaspContainer(std::string title = "", jaspObjectType type = jaspObjectType::container) : jaspObject(type, title)
	{
#ifdef JASP_RESULTS_DEBUG_TRACES
		std::cout << "JASPcontainer constructor for title: " << _title << std::endl;
#endif
	}

	jaspContainer(const jaspContainer& that) = delete;

	std::string dataToString(std::string prefix = "")						const	override;
	std::string toHtml()													const	override;

	void			insert(std::string field, jaspObject * value);
	jaspObject *	at(std::string field);

	Json::Value	metaEntry(jaspObject * oldResult)							const	override;
	Json::Value	dataEntry(jaspObject * oldResult, std::string & errorMsg)	const	override;

	std::string getCommonDenominatorMetaType() const;

	int	length() { return _data.size(); }

	void childFinalizedHandler(jaspObject *child)									override;

	Json::Value convertToJSON()												const	override;
	void		convertFromJSON_SetFields(Json::Value in)							override;
	void		checkDependenciesChildren(Json::Value currentOptions)				override;

	void		completeChildren();
	void		letChildrenRun();
	void		setError()															override;
	void		setError(std::string message)										override;
	void		renderPlotsOfChildren();

	bool		containsNonContainer();
	bool		canShowErrorMessage()										const	override;

	bool		_initiallyCollapsed = false;

	static std::vector<std::string>				convertSortedDataFieldsToStringVector(std::vector<std::pair<double, std::string>> sortvec, bool removeDuplicates = false);
	std::vector<std::pair<double, std::string>> getSortedDataFieldsSortVector()														const;
	std::vector<std::string>					getSortedDataFields()																const;
	std::vector<std::string>					getSortedDataFieldsWithOld(jaspContainer * oldResult)								const;
	jaspObject *								getJaspObjectNewOrOld(std::string fieldName, jaspContainer * oldResult)				const;
	jaspObject *								getJaspObjectFromData(std::string fieldName)										const;
	bool										jaspObjectComesFromOldResults(std::string fieldName, jaspContainer * oldResult)		const;

	jaspObject *								findObjectWithNestedNameVector(const std::vector<std::string> &uniqueName, const size_t position = 0);
	jaspObject *								findObjectWithUniqueNestedName(const std::string & uniqueNestedName);

protected:
	std::map<std::string, jaspObject*>	_data;
	std::map<std::string, int>			_data_order;
	int									_order_increment = 0;

};
