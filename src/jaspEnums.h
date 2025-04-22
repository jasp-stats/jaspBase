#pragma once
#include <string>
#include "enumutilities.h"

DECLARE_ENUM(jaspObjectType,        unknown, container, table, plot, list, results, html, state, column, qmlSource, report);
DECLARE_ENUM(jaspColumnType,        unknown, scale, ordinal, nominal, nominalText);
DECLARE_ENUM(jaspTableColumnType,   unknown, null, string, logical, integer, number, various, composite, mixed); 

jaspObjectType jaspObjectTypeStringToObjectType(std::string type);
