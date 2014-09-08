/** \file lispstring.h
 *  Defining a string class.
 */


#ifndef YACAS_LISPSTRING_H
#define YACAS_LISPSTRING_H

#include "yacasbase.h"
#include "refcount.h"

#include <cstring>
#include <string>

class LispStringSmartPtr;


/** \class LispString : zero-terminated byte-counted string.
 * Also keeps a reference count for any one interested.
 */
class LispString : public std::string
{
public:
    // Constructors
    // Use the assignment operators to set the string after this.
    inline LispString();
    explicit LispString(const std::string&);
    explicit LispString(const LispString& aString);
    explicit LispString(const LispChar* aString);

    // Assignment
    LispString& operator = (const LispString& aString);
    LispString& operator = (const LispChar* aString);

    // Comparison
    // If the string is in the hash table it is faster to compare the
    // pointers to the strings (instead of calling this routine),
    // since in that case if they are equal they should in fact be
    // literally the same object.
    LispInt operator==(const LispString& aString) const;

public:
    ReferenceCount iReferenceCount;
};


// LispString inline functions.

inline LispString& LispString::operator=(const LispString& aString)
{
  assign(aString.c_str());
  return *this;
}

inline LispString& LispString::operator=(const LispChar* aString)
{
  assign(aString);
  return *this;
}

inline LispString::LispString(const std::string& s):
    std::string(s),
    iReferenceCount(0)
{
}

inline LispString::LispString(const LispString& s):
    std::string(s),
    iReferenceCount(0)
{
}

inline LispString::LispString(const LispChar* s):
    std::string(s),
    iReferenceCount(0)
{
}

inline LispString::LispString():
    iReferenceCount(0)
{
}

/** \class LispStringSmartPtr for managing strings outside
 of normal objects. This is the equivalent of LispPtr, maintaining
 a reference count for the string object.
 */
class LispStringSmartPtr
{
public:
  // Default constructor (not explicit, so it auto-initializes)
  LispStringSmartPtr() : iString(nullptr) {}

  // Construct from pointer to LispString
  LispStringSmartPtr(LispString * aString) : iString(nullptr)
  {
    this->operator=(aString);
  }

  // Copy constructor
  LispStringSmartPtr(const LispStringSmartPtr& aOther) : iString()
  {
    this->operator=(aOther.iString);
  }

  // Destructor
  ~LispStringSmartPtr();

  // Assignment from pointer.  (PDG - new method)
  // (we return void, not *this).
  LispStringSmartPtr& operator=(LispString * aString);

  // Assignment from another (the *default* simply assigns members, not what we want).
  // (we return void, not *this).
  LispStringSmartPtr& operator=(const LispStringSmartPtr &aOther) { this->operator=(aOther.iString); return *this; }

  // Expected pointer behavior.
  operator LispString*()    const { return  iString; }  // implicit conversion to pointer to T
  LispString *operator->()  const { return  iString; }  // so (smartPtr->member) accesses T's member

  // Operators below are not used yet, so they are commented out. If you want to use them you need to test if they work.
  //LispString &operator*() const { return *iString; }  // so (*smartPtr) is a reference to T
  //LispString *ptr()       const { return  iString; }  // so (smartPtr.ptr()) returns the pointer to T (boost calls this method 'get')
  //bool operator!()        const { return !iString; }  // is null pointer

private:
  LispString * iString;
};

#endif


