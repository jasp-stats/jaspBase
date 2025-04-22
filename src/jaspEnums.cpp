#define ENUM_DECLARATION_CPP
#include "jaspEnums.h"

jaspObjectType jaspObjectTypeStringToObjectType(std::string type)
{
	try			{ return jaspObjectTypeFromString(type); }
	catch(...)	{ return jaspObjectType::unknown; }
}