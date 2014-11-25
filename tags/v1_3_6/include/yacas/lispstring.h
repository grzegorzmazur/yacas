/** \file lispstring.h
 *  Defining a string class.
 */


#ifndef YACAS_LISPSTRING_H
#define YACAS_LISPSTRING_H

#include "refcount.h"

#include <string>

class LispStringSmartPtr;


/** \class LispString : zero-terminated byte-counted string.
 * Also keeps a reference count for any one interested.
 */
class LispString : public std::string
{
public:
    explicit LispString(const std::string& = "");

public:
    mutable ReferenceCount iReferenceCount;
};


inline LispString::LispString(const std::string& s):
    std::string(s),
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
  LispStringSmartPtr(const LispString* aString) : iString(nullptr)
  {
    this->operator = (aString);
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
  LispStringSmartPtr& operator = (const LispString* aString);

  // Assignment from another (the *default* simply assigns members, not what we want).
  // (we return void, not *this).
  LispStringSmartPtr& operator=(const LispStringSmartPtr &aOther) { this->operator=(aOther.iString); return *this; }

  // Expected pointer behavior.
  operator const LispString*()    const { return  iString; }  // implicit conversion to pointer to T
  const LispString* operator->()  const { return  iString; }  // so (smartPtr->member) accesses T's member

  // Operators below are not used yet, so they are commented out. If you want to use them you need to test if they work.
  //LispString &operator*() const { return *iString; }  // so (*smartPtr) is a reference to T
  //LispString *ptr()       const { return  iString; }  // so (smartPtr.ptr()) returns the pointer to T (boost calls this method 'get')
  //bool operator!()        const { return !iString; }  // is null pointer

private:
  const LispString* iString;
};

inline
LispStringSmartPtr& LispStringSmartPtr::operator=(const LispString* aString)
{
  // Increment first.
  if (aString)
    ++aString->iReferenceCount;
  if (iString)
  {
    --iString->iReferenceCount;
    if (iString->iReferenceCount == 0) delete iString;
  }
  iString = aString;
  return *this;
}

inline
LispStringSmartPtr::~LispStringSmartPtr()
{
  if (iString && !--iString->iReferenceCount)
    delete iString;
}


#endif
