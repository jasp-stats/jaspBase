#pragma once

// CORE (R-free) version of jaspList.h. R-facing index/insert dispatch (1-based
// [[ints]] / fieldnames via Rcpp::RObject), setRows(Rcpp::List) and the
// jaspList_Interface + JASPLIST_MODULE_EXPORT live in
// src/adapters/rcpp/rcppInterfaces.h. Storage is plain vector/map and JSON is
// built here, so hosts (R, Python) only differ in how they index into it.

#include "jaspObject.h"

template<typename T>
class jaspList : public jaspObject
{
public:
	jaspList(std::string title = "") : jaspObject(jaspObjectType::list, title), _dummyVal()
	{
		allocatedObjects->erase(this); // lists are never newed!
	}

	///zero-based index insert; resizes the row vector when needed
	void insertIndex(size_t row, T value)
	{
		if(_rows.size() <= row)
			_rows.resize(row+1);

		_rows[row] = value;

		notifyParentOfChanges();
	}

	void insertField(std::string fieldName, T value)
	{
		_field_to_val[fieldName] = value;

		notifyParentOfChanges();
	}

	void add(T value)
	{
		_rows.push_back(value);
		notifyParentOfChanges();
	}

	///zero-based index access; returns a default value when out of range.
	T atIndex(size_t row) const
	{
		if(row >= _rows.size())
			return T();

		return _rows[row];
	}

	///field access; throws std::out_of_range for unknown fields (today's R behavior).
	T atField(std::string fieldName) const
	{
		return _field_to_val.at(fieldName);
	}

	std::string dataToString(std::string prefix) const override
	{
		std::stringstream out;
		out << "{ ";

		if(_rows.size() > 0)
		{
			if(_field_to_val.size() > 0)
				out << "\n" ;

			out << "vec: [";

			int count = 0;
			for(auto row : _rows)
				out << (count++ > 0 ? ", " : "") << '"' << row << '"';

			out << "]";
		}

		if(_field_to_val.size() > 0)
		{
			std::string newPrefix = "\t" + prefix;

			if(_rows.size() > 0)
				out << "\n" << newPrefix;

			out << "map: {";

			int count = 0;
			for(auto key : _field_to_val)
				out << (count++ > 0 ? ",\n": "\n") << newPrefix << "\t\"" << key.first << "\": " << '"' << key.second<< '"';

			out << "\n" << newPrefix << "}";
		}

		out << prefix << "}";

		return out.str();
	}

	///clears the rows and appends the named entries to the existing fields
	///(no clearing of _field_to_val, no notify: exactly today's setRows)
	void setRows(const std::vector<T> & vec, const std::map<std::string, T> & fields = {})
	{
		_rows.clear();
		_rows.insert(_rows.end(), vec.begin(), vec.end());

		for(const auto & keyval : fields)
			_field_to_val[keyval.first] = keyval.second;
	}

	size_t rowCount()	const { return _rows.size(); }
	size_t fieldCount()	const { return _field_to_val.size(); }

	///using [] (in c++) will give you normal zero-based array but also grows the vector if your request lies outside of it
	T & operator[](size_t index)
	{
		if(_rows.size() <= index)
			_rows.resize(index + 1); //Yes we create new entries like this but that avoids a whole lot of errors
		return _rows[index];
	}

	const T _dummyVal;

	const T & operator[](size_t index) const
	{
		if(_rows.size() <= index)
			return _dummyVal;
		return _rows.at(index);
	}

			T & operator[](std::string field)				{ return _field_to_val[field];		}
	const	T & operator[](std::string field)		const	{ return _field_to_val.count(field) > 0 ? _field_to_val.at(field) : _dummyVal; }

	bool containsField(std::string field)	const	{ return _field_to_val.count(field) > 0; }


	Json::Value convertToJSON() const override
	{
		Json::Value obj		= jaspObject::convertToJSON();
		obj["rows"]			= Json::arrayValue;
		for(auto r : _rows)
			obj["rows"].append((T)r);

		obj["fields"]		= Json::objectValue;

		for(auto k : _field_to_val)
			obj["fields"][k.first] = k.second;

		if(std::is_same<T, std::string>::value)	obj["listType"] = "string";
		else if(std::is_same<T, double>::value)	obj["listType"] = "double";
		else if(std::is_same<T, int>::value)	obj["listType"] = "int";
		else if(std::is_same<T, bool>::value)	obj["listType"] = "bool";
		else									obj["listType"] = "unknown";

		return obj;
	}

	//Specialized in cpp
	inline T convertStringFromJson(	Json::Value value)		{ return T(); }
	inline T convertDoubleFromJson(	Json::Value value)		{ return T(); }
	inline T convertIntFromJson(	Json::Value value)		{ return T(); }
	inline T convertBoolFromJson(	Json::Value value)		{ return T(); }

	void		convertFromJSON_SetFields(Json::Value in) override
	{
		jaspObject::convertFromJSON_SetFields(in);

		std::string listType = in.get("listType", "unknown").asString();

		if(		listType == "unknown"												||
				(std::is_same<T, std::string>::value	&& listType != "string")	||
				(std::is_same<T, double>::value			&& listType != "double")	||
				(std::is_same<T, bool>::value			&& listType != "bool")		||
				(std::is_same<T, int>::value			&& listType != "int")		)
			throw std::logic_error("Wrong listtype for conversion from JSON to jaspList!");

		_rows.clear();
		for(auto & row : in.get("rows", Json::arrayValue))
			if(std::is_same<T, std::string>::value)	_rows.push_back(convertStringFromJson(row));
			else if(std::is_same<T, double>::value)	_rows.push_back(convertDoubleFromJson(row));
			else if(std::is_same<T, int>::value)	_rows.push_back(convertIntFromJson(row));
			else if(std::is_same<T, bool>::value)	_rows.push_back(convertBoolFromJson(row));

		Json::Value fields(in.get("fields", Json::objectValue));
		_field_to_val.clear();
		for(auto & memberName : fields.getMemberNames())
		{
			Json::Value fieldVal = fields[memberName];

			if(std::is_same<T, std::string>::value)	_field_to_val[memberName] = convertStringFromJson(fieldVal);
			else if(std::is_same<T, double>::value)	_field_to_val[memberName] = convertDoubleFromJson(fieldVal);
			else if(std::is_same<T, int>::value)	_field_to_val[memberName] = convertIntFromJson(fieldVal);
			else if(std::is_same<T, bool>::value)	_field_to_val[memberName] = convertBoolFromJson(fieldVal);
		}
	}

private:
	std::map<std::string, T> _field_to_val;
	std::vector<T> _rows;

};

template <> inline std::string	jaspList<std::string>::	convertStringFromJson(Json::Value	value)	{ return value.asString();	}
template <>	inline double		jaspList<double>::		convertDoubleFromJson(Json::Value	value)	{ return value.asDouble();	}
template <>	inline int			jaspList<int>::			convertIntFromJson(Json::Value		value)	{ return value.asInt();		}
template <>	inline bool			jaspList<bool>::		convertBoolFromJson(Json::Value		value)	{ return value.asBool();	}

typedef jaspList<std::string>	jaspStringlist;
typedef jaspList<double>		jaspDoublelist;
typedef jaspList<int>			jaspIntlist;
typedef jaspList<bool>			jaspBoollist;
