
#include "yacasprivate.h"
#include "lispatom.h"
#include "lispassert.h"
#include "lisperror.h"
#include "numbers.h"
#include "standard.h"

#ifdef YACAS_DEBUG
#include <stdio.h> //DEBUG
#endif

/// construct an atom from a string representation.
LispObject* LispAtom::New(LispEnvironment& aEnvironment, LispCharPtr aString)
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

LispAtom::LispAtom(LispStringPtr aString)
{
    LISPASSERT(aString!=NULL);
    iString = aString;
    aString->IncreaseRefCount();
    CHECKPTR(iString);
}
LispAtom::~LispAtom()
{
    iString->DecreaseRefCount();
}


LispStringPtr LispAtom::String() 
{
    CHECKPTR(iString);
    return iString;
}

LispObject* LispAtom::Copy(LispInt aRecursed)
{
    LispObject *copied = NEW LispAtom(iString);
#ifdef YACAS_DEBUG
    copied->SetFileAndLine(iFileName, iLine);
#endif
    return copied;
}


LispObject* LispAtom::SetExtraInfo(LispPtr& aData)
{
    LispObject* result = NEW LispAnnotatedObject<LispAtom>(this);
    result->SetExtraInfo(aData);
    return result;
}



LispSubList* LispSubList::New(LispObject* aSubList)
{
    LispSubList* self = NEW LispSubList(aSubList);
    Check(self!=NULL,KLispErrNotEnoughMemory);
    return self;
}

LispSubList::LispSubList(LispObject* aSubList)
{
    iSubList.Set(aSubList);
}

LispPtr* LispSubList::SubList()
{
    return &iSubList;
}

LispStringPtr LispSubList::String()
{
    return NULL;
}


LispObject* LispSubList::Copy(LispInt aRecursed)
{
    //TODO recursed copy needs to be implemented still
    LISPASSERT(aRecursed == 0);
    LispObject *copied = NEW LispSubList(iSubList.Get());
#ifdef YACAS_DEBUG
    copied->SetFileAndLine(iFileName, iLine);
#endif
    return copied;
}


LispObject* LispSubList::SetExtraInfo(LispPtr& aData)
{
    LispObject* result = NEW LispAnnotatedObject<LispSubList>(this);
    result->SetExtraInfo(aData);
    return result;
}



// A destructor for lists that is less taxing for stacks :-)
// Eg. deleting a list deletes the entire sublist also, in
// a tail-recursive way...
LispSubList::~LispSubList()
{
    if (iSubList.Get() != NULL)
    {
        LispPtr next;
        LispIterator iter(iSubList);
        LispBoolean busy = (iter()->ReferenceCount() == 1);
        while (busy) // while there are things to delete...
        {
            LispPtr nextToDelete;
            // Make sure "next" holds the tail of the list
            nextToDelete.Set(iter()->Next().Get());
            // Separate out the current element...
            if (iter()->ReferenceCount() == 1)
            {// Destructive operation only if necessary...
                iter()->Next().Set(NULL);
                // And delete it.
                iter.Ptr()->Set(NULL);
            }
            else
                busy=LispFalse;
            next.Set(nextToDelete.Get());
            iter = next;
            if (iter() == NULL)
                busy=LispFalse;
        }
    }
}




LispGenericClass* LispGenericClass::New(GenericClass* aClass)
{
    LispGenericClass* self = NEW LispGenericClass(aClass);
    Check(self!=NULL,KLispErrNotEnoughMemory);
    return self;
}
LispStringPtr LispGenericClass::String()
{
    return NULL;
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

LispObject* LispGenericClass::Copy(LispInt aRecursed)
{
    //TODO real copy!
    LispObject *copied = NEW LispGenericClass(iClass);
#ifdef YACAS_DEBUG
    copied->SetFileAndLine(iFileName, iLine);
#endif
    return copied;
}

LispObject* LispGenericClass::SetExtraInfo(LispPtr& aData)
{
    LispObject* result = NEW LispAnnotatedObject<LispGenericClass>(this);
    result->SetExtraInfo(aData);
    return result;
}

#ifndef NO_USE_BIGFLOAT

    /// construct from another LispNumber
LispNumber::LispNumber(BigNumber* aNumber,LispStringPtr aString)
{
  iString = aString;
  iNumber = aNumber;
}


    /// construct from a BigNumber; the string representation will be absent until requested
LispNumber::LispNumber(BigNumber* aNumber)
{
  iString = NULL;
  iNumber =aNumber;
}

    /// construct from a decimal string representation (also create a number object) and use aBasePrecision digits
LispNumber::LispNumber(LispStringPtr aString, LispInt aBasePrecision)
{
  iString = aString;
  iNumber = NULL;	// purge whatever it was
  // create a new BigNumber object out of iString, set its precision in digits
  Number(aBasePrecision);
}

/// return a string representation in decimal
LispStringPtr LispNumber::String() 
{
  if (iString.Ptr() == NULL)
  {
    LISPASSERT(iNumber.Ptr() != NULL);	// either the string is null or the number but not both
    LispString *str = NEW LispString;
    // export the current number to string and store it as LispNumber::iString
    iNumber->ToString(*str, bits_to_digits(iNumber->GetPrecision(),BASE10), BASE10);
    iString = str;	
  }
  return iString.Ptr();
}

LispNumber::~LispNumber()
{
  iNumber = NULL;
}
LispObject* LispNumber::Copy(LispInt aRecursed)
{
    LispObject *copied;
    copied = NEW LispNumber(iNumber.Ptr(), iString.Ptr());

#ifdef YACAS_DEBUG
    copied->SetFileAndLine(iFileName, iLine);
#endif
    return copied;
}

    /// Return a BigNumber object.
// Will create a BigNumber object out of a stored string, at given precision (in decimal) - that's why the aPrecision argument must be here - but only if no BigNumber object is already present
BigNumber* LispNumber::Number(LispInt aBasePrecision)
{
  if (iNumber.Ptr() == NULL)
  {	// create and store a BigNumber out of string
    LISPASSERT(iString.Ptr() != NULL);
    RefPtr<LispString> str;
    str = iString.Ptr();
    // aBasePrecision is in digits, not in bits, ok
    iNumber = NEW BigNumber(str->String(), aBasePrecision, BASE10);
  }

  // check if the BigNumber object has enough precision, if not, extend it
  // (applies only to floats). Note that iNumber->GetPrecision() might be < 0
  else if (!iNumber->IsInt() && iNumber->GetPrecision() < (LispInt)digits_to_bits(aBasePrecision, BASE10))
  {
    if (iString.Ptr())
    {// have string representation, can extend precision
      iNumber->SetTo(iString.Ptr()->String(),aBasePrecision, BASE10);
    }
    else
    {
	// do not have string representation, cannot extend precision!
    }
  }
  return iNumber.Ptr();
}


LispObject* LispNumber::SetExtraInfo(LispPtr& aData)
{
    LispObject* result = NEW LispAnnotatedObject<LispNumber>(this);
    result->SetExtraInfo(aData);
    return result;
}

#endif
