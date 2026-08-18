#pragma once

// CORE (R-free) version of jaspReport.h. jaspReport_Interface moved to
// src/adapters/rcpp/rcppInterfaces.h; analysisId comes from jaspHost.

#include "jaspObject.h"

class jaspReport : public jaspObject
{
public:
  jaspReport(std::string text = "", bool report = false) 
  : jaspObject(jaspObjectType::report, ""), _rawText(text), _report(report)
  {}

					~jaspReport() {}

	std::string 	dataToString(std::string prefix="")			const	override;
	std::string 	toHtml()									const	override;
	std::string		toTopHtml()									const;

	Json::Value		metaEntry()									const	override { return constructMetaEntry("reportNode"); }
	Json::Value		dataEntry(std::string & errorMessage)		const	override;

	Json::Value 	convertToJSON()								const	override;
	void			convertFromJSON_SetFields(Json::Value in)			override;

	void			setText(std::string newRawText) 					{ _rawText 	= newRawText;	}
	std::string 	getText() 									const 	{ return _rawText;			}

	std::string 	_rawText;
	bool			_report;

	static	void	totalWarningsClear() { _totalWarnings = 0; 	}
			void	totalWarningsInc();

private:
	size_t			_warningIndex = 0;
	static	size_t	_totalWarnings;
};
