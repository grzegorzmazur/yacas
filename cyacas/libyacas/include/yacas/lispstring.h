/** \file lispstring.h
 *  Defining a string class.
 */


#ifndef YACAS_LISPSTRING_H
#define YACAS_LISPSTRING_H

#include "refcount.h"

#include <string>

/** \class LispString : zero-terminated byte-counted string.
 * Also keeps a reference count for any one interested.
 */
class LispString : public std::string
{
public:
    explicit LispString(const std::string& = "");

public:
    mutable unsigned iReferenceCount;
};


inline LispString::LispString(const std::string& s):
    std::string(s),
    iReferenceCount(0)
{
}

typedef RefPtr<const LispString> LispStringSmartPtr;

#endif
