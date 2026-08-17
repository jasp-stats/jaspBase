#pragma once

// CORE (R-free) version of jaspState.h. Object payloads travel through the
// jaspHost object store as opaque std::any handles (Rcpp::RObject in the R
// build, py::object in the Python build); jaspState_Interface in
// src/adapters/rcpp/rcppInterfaces.h keeps the R-facing SEXP API.

#include "jaspObject.h"
#include "jaspHost.h"
#include <any>

class jaspState : public jaspObject
{
public:
	jaspState(std::string title = "") : jaspObject(jaspObjectType::state, title) { initEnvName(); }

	void			setObject(std::any obj);
	std::any		getObject();
	bool			hasObject() const;

	Json::Value		convertToJSON()								const	override;
	void			convertFromJSON_SetFields(Json::Value in)			override;
	std::string		dataToString(std::string prefix)			const	override;
	std::string		_envName;

private:
	void initEnvName();
};
