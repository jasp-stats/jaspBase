#pragma once

// CORE (R-free) version of jaspHtml.h. toRObject() moved to
// src/adapters/rcpp/rcppToRObject.cpp, jaspHtml_Interface to
// src/adapters/rcpp/rcppInterfaces.h.

#include "jaspObject.h"

class jaspHtml : public jaspObject
{
public:
  jaspHtml(std::string text = "", std::string elementType = "p", std::string maxWidth="15cm", std::string Class = "") : jaspObject(jaspObjectType::html, ""), _rawText(text), _elementType(elementType), _class(Class), _maxWidth(maxWidth) {}

	~jaspHtml() {}

	std::string dataToString(std::string prefix="")			const	override;
	std::string toHtml()									const	override;

	Json::Value	metaEntry()									const	override { return constructMetaEntry("htmlNode"); }
	Json::Value	dataEntry(std::string & errorMessage)		const	override;

	std::string _rawText, _elementType, _class, _maxWidth;

	Json::Value convertToJSON()								const	override;
	void		convertFromJSON_SetFields(Json::Value in)			override;

			std::string convertTextToHtml(  const std::string text)		const;
	static	std::string sanitizeTextForHtml(const std::string text);

    void setText(std::string newRawText);
    std::string getText();
    std::string getHtml();
};
