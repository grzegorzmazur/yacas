#ifndef STRING_UTILS_H
#define STRING_UTILS_H

#include <algorithm>
#include <functional>
#include <cctype>
#include <locale>
#include <string>

inline
std::string stringify(const std::string& s)
{
    return "\"" + s + "\"";
}

inline
std::string& ltrim(std::string& s)
{
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int c) { return !std::isspace(c); }));
    return s;
}

inline
std::string& rtrim(std::string& s)
{
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) { return !std::isspace(ch); }).base(), s.end());
    return s;
}

inline
std::string& trim(std::string& s)
{
    return ltrim(rtrim(s));
}

#endif

