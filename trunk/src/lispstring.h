/** \file lispstring.h
 *  Defining a string class.
 */


#ifndef YACAS_LISPSTRING_H
#define YACAS_LISPSTRING_H

#include <cstring>

#include "yacasbase.h"
#include "grower.h"
#include "refcount.h"

class LispStringSmartPtr;


/** \class LispString : zero-terminated byte-counted string.
 * Also keeps a reference count for any one interested.
 * LispString is derived from CArrayGrower, so the function
 * Size returns the length of the buffer. Since the string
 * is also zero-terminated (for better interfacing with the normal
 * c functions), the string length is Size()-1.
 *
 * This class also allows the string to point to a buffer which is owned
 * by another part of the system, in which case it cannot be resized.
 * The array will then not be freed by this class.
 */
class LispString : public CArrayGrower<LispChar,ArrOpsPOD<LispChar> >
{
public:
    // Constructors
    // Use the assignment operators to set the string after this.
    inline LispString();
    inline LispString(const LispString &aString);
    inline LispString(const LispChar* aString);

    // Assignment
    inline LispString& operator = (const LispString& aString);
    inline LispString& operator = (const LispChar* aString);

    // Assignments (with modifications).
    // Set string by taking part of another string.
    void SetStringCounted(const LispChar * aString, LispInt aLength);
    // Set string from other string, adding quotes around the string.
    void SetStringUnStringified(const LispChar * aString);
    // Set string from other string, removing quotes around the string.
    void SetStringStringified(const LispChar * aString);

    // Access
    const LispChar * c_str() const;  // pointer to asciz 'C-string'

    // Comparison
    // If the string is in the hash table it is faster to compare the
    // pointers to the strings (instead of calling this routine),
    // since in that case if they are equal they should in fact be
    // literally the same object.
    LispInt operator==(const LispString& aString);

private:
    void SetString(const LispChar * aString);
public:
    ReferenceCount iReferenceCount;
};


/** \class LispStringSmartPtr for managing strings outside
 of normal objects. This is the equivalent of LispPtr, maintaining
 a reference count for the string object.
 */
class LispStringSmartPtr
{
public:
  // Default constructor (not explicit, so it auto-initializes)
  LispStringSmartPtr() : iString(NULL) {}

  // Construct from pointer to LispString
  LispStringSmartPtr(LispString * aString) : iString(NULL)
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

#include "lispstring.inl"
#endif


