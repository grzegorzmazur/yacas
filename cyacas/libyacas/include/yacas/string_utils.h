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
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
    return s;
}

inline
std::string& rtrim(std::string& s)
{
    s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
    return s;
}

inline
std::string& trim(std::string& s)
{
    return ltrim(rtrim(s));
}

#endif

