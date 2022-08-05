#pragma once
#include "jaspObject.h"

class jaspReport : public jaspObject
{
public:
  jaspReport(Rcpp::String text = "", bool report = false) 
  : jaspObject(jaspObjectType::report, ""), _rawText(jaspNativeToUtf8(text)), _report(report)
  {}

	~jaspReport() {}

	std::string dataToString(std::string prefix="")			const	override;
	std::string toHtml()									const	override;

	Json::Value	metaEntry()									const	override { return constructMetaEntry("reportNode"); }
	Json::Value	dataEntry(std::string & errorMessage)		const	override;

	Json::Value convertToJSON()								const	override;
	void		convertFromJSON_SetFields(Json::Value in)			override;

	void		setText(Rcpp::String newRawText) 					{ _rawText 	= jaspNativeToUtf8(newRawText);	}
	std::string getText() 									const 	{ return _rawText;			}

	std::string _rawText;
	bool		_report;
};



class jaspReport_Interface : public jaspObject_Interface
{
public:
	jaspReport_Interface(jaspObject * dataObj) : jaspObject_Interface(dataObj) {}

    void			setText(Rcpp::String newRawText) { 			static_cast<jaspReport *>(myJaspObject)->setText(newRawText); }
    Rcpp::String	getText() 						{ return 	static_cast<jaspReport *>(myJaspObject)->getText(); }

	JASPOBJECT_INTERFACE_PROPERTY_FUNCTIONS_GENERATOR(jaspReport, bool,	_report,	Report)
};

RCPP_EXPOSED_CLASS_NODECL(jaspReport_Interface)

