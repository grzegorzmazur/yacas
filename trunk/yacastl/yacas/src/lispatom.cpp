
#include "yacasprivate.h"
#include "lispatom.h"
#include "lispassert.h"
#include "lisperror.h"
#include "numbers.h"
#include "standard.h"
#include "codecomment.h"

#define STR(tokens) #tokens
#define SHOWSTR(ctce) #ctce " = " STR(ctce)
namespace{CodeComment varname3(SHOWSTR(HAS_NEW_AtomImpl));}
namespace{CodeComment varname4(SHOWSTR(HAS_NEW_LispPtrArray));}


/// construct an atom from a string representation.
LispObject* LispAtom::New(LispEnvironment& aEnvironment, const LispChar * aString)
{
  LispObject* self;
#ifndef NO_USE_BIGFLOAT
  if (IsNumber(aString,LispTrue))	// check if aString is a number (int or float)
  {
    /// construct a number from a decimal string representation (also create a number object)
    self = NEW LispNumber(NEW LispString(aString), aEnvironment.Precision());
  }
  else
#endif
  {
    self = NEW LispAtom(aEnvironment.HashTable().LookUp(aString));
  }
  Check(self!=NULL,KLispErrNotEnoughMemory);
  return self;
}

LispAtom::LispAtom(LispString * aString)
{
    LISPASSERT(aString!=NULL);
    iString = aString;
#if !HAS_NEW_AtomImpl
    ++aString->iReferenceCount;
#endif
    CHECKPTR(iString);
}

LispAtom::LispAtom(const LispAtom& other) : ASuper(other), iString(other.iString)
{
#if !HAS_NEW_AtomImpl
    ++iString->iReferenceCount;
#endif
}

LispAtom::~LispAtom()
{
#if !HAS_NEW_AtomImpl
    --iString->iReferenceCount;
#endif
}


LispString * LispAtom::String() 
{
    CHECKPTR(iString);
    return iString;
}

//------------------------------------------------------------------------------
// LispSublist methods

LispSubList* LispSubList::New(LispObject* aSubList)
{
    LispSubList* self = NEW LispSubList(aSubList);
    Check(self!=NULL,KLispErrNotEnoughMemory);
    return self;
}

/*
LispSubList::LispSubList(LispObject* aSubList)
{
    iSubList = (aSubList);
}
*/

#if 0
LispPtr* LispSubList::SubList()
{
    return &iSubList;
}
#endif

#if 0
LispString * LispSubList::String()
{
    return NULL;
}
#endif

// A destructor for lists that is less taxing for stacks :-)
// Eg. deleting a list deletes the entire sublist also, in
// a tail-recursive way...
LispSubList::~LispSubList()
{
    if (!!iSubList)
    {
        LispPtr next;
        LispIterator iter(iSubList);
        LispBoolean busy = (iter.getObj()->iReferenceCount == 1);
        while (busy) // while there are things to delete...
        {
			// TODO: woof -- fix this ugliness!
            LispPtr nextToDelete;
            // Make sure "next" holds the tail of the list
            nextToDelete = (iter.getObj()->Nixed());
            // Separate out the current element...
            if (iter.getObj()->iReferenceCount == 1)
            {// Destructive operation only if necessary...
                iter.getObj()->Nixed() = (NULL);
                // And delete it.
                (*iter) = (NULL);
            }
            else
                busy=LispFalse;
            next = (nextToDelete);
            iter = next;
            if (!iter.getObj())
                busy=LispFalse;
        }
    }
}

//------------------------------------------------------------------------------
// LispGenericClass methods

LispGenericClass* LispGenericClass::New(GenericClass* aClass)
{
    LispGenericClass* self = NEW LispGenericClass(aClass);
    Check(self!=NULL,KLispErrNotEnoughMemory);
    return self;
}

LispGenericClass::LispGenericClass(GenericClass* aClass)
{
    LISPASSERT(aClass!=NULL);
    iClass = aClass;
    aClass->iReferenceCount++;
}

LispGenericClass::~LispGenericClass()
{
    iClass->iReferenceCount--;
    if (iClass->iReferenceCount == 0)
    {
        delete iClass;
    }
    iClass=NULL;
}

GenericClass* LispGenericClass::Generic()
{
    return iClass;
}

//------------------------------------------------------------------------------
// LispNumber methods - proceed at your own risk 

#ifndef NO_USE_BIGFLOAT

/// return a string representation in decimal
LispString * LispNumber::String() 
{
  if (!iString)
  {
#if !HAS_NEW_AtomImpl
    LISPASSERT(iNumber.ptr());	// either the string is null or the number but not both
#else
    LISPASSERT(iNumber);	// either the string is null or the number but not both
#endif
    LispString *str = NEW LispString;
    // export the current number to string and store it as LispNumber::iString
    iNumber->ToString(*str, bits_to_digits(MAX(1,iNumber->GetPrecision()),BASE10), BASE10);
    iString = str;	
  }
#if !HAS_NEW_AtomImpl
  return iString;
#else
  return iString;
#endif
}

/// Return a BigNumber object.
// Will create a BigNumber object out of a stored string, at given precision (in decimal) - that's why the aPrecision argument must be here - but only if no BigNumber object is already present
BigNumber* LispNumber::Number(LispInt aBasePrecision)
{
  if (!iNumber)
  {	// create and store a BigNumber out of string
#if !HAS_NEW_AtomImpl
    LISPASSERT(iString.ptr());
    RefPtr<LispString> str;
    str = iString;
#else
    LISPASSERT(iString);
    RefPtr<LispString> str(iString);	// TODO: woof -- PDG -- what's going on here?
#endif
    // aBasePrecision is in digits, not in bits, ok
    iNumber = NEW BigNumber(str->c_str(), aBasePrecision, BASE10);
  }

  // check if the BigNumber object has enough precision, if not, extend it
  // (applies only to floats). Note that iNumber->GetPrecision() might be < 0
  else if (!iNumber->IsInt() && iNumber->GetPrecision() < (LispInt)digits_to_bits(aBasePrecision, BASE10))
  {
#if !HAS_NEW_AtomImpl
    if (!!iString)
    {// have string representation, can extend precision
      iNumber->SetTo(iString->c_str(),aBasePrecision, BASE10);
    }
#else
    if (iString)
    {// have string representation, can extend precision
      iNumber->SetTo(iString->c_str(),aBasePrecision, BASE10);
    }
#endif
    else
    {
	// do not have string representation, cannot extend precision!
    }
  }
  return iNumber;
}

#endif
