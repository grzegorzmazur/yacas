
#include "yacasprivate.h"
#include "lispatom.h"
#include "lispassert.h"
#include "lisperror.h"
#include "numbers.h"
#include "standard.h"


/// construct an atom from a string representation.
LispObject* LispAtom::New(LispEnvironment& aEnvironment, const LispChar * aString)
{
  LispObject* self;
  if (IsNumber(aString,LispTrue))  // check if aString is a number (int or float)
  {
    /// construct a number from a decimal string representation (also create a number object)
    self = NEW LispNumber(NEW LispString(aString), aEnvironment.Precision());
  }
  else
  {
    self = NEW LispAtom(aEnvironment.HashTable().LookUp(aString));
  }
  Check(self!=NULL,KLispErrNotEnoughMemory);
  return self;
}

LispAtom::LispAtom(LispString * aString) : iString(aString)
{
    LISPASSERT(aString!=NULL);
    ++aString->iReferenceCount;
    CHECKPTR(iString);
}

LispAtom::LispAtom(const LispAtom& other) : ASuper(other), iString(other.iString)
{
  ++iString->iReferenceCount;
}

LispAtom::~LispAtom()
{
  --iString->iReferenceCount;
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

LispGenericClass::LispGenericClass(GenericClass* aClass) : iClass(aClass)
{
    LISPASSERT(aClass!=NULL);
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


/// return a string representation in decimal
LispString * LispNumber::String()
{
  if (!iString)
  {
    LISPASSERT(iNumber.ptr());  // either the string is null or the number but not both
    LispString *str = NEW LispString;
    // export the current number to string and store it as LispNumber::iString
    iNumber->ToString(*str, bits_to_digits(MAX(1,iNumber->GetPrecision()),BASE10), BASE10);
    iString = str;
  }
  return iString;
}

/// Return a BigNumber object.
// Will create a BigNumber object out of a stored string, at given precision (in decimal) - that's why the aPrecision argument must be here - but only if no BigNumber object is already present
BigNumber* LispNumber::Number(LispInt aBasePrecision)
{
  if (!iNumber)
  {  // create and store a BigNumber out of string
    LISPASSERT(iString.ptr());
    RefPtr<LispString> str;
    str = iString;
    // aBasePrecision is in digits, not in bits, ok
    iNumber = NEW BigNumber(str->c_str(), aBasePrecision, BASE10);
  }

  // check if the BigNumber object has enough precision, if not, extend it
  // (applies only to floats). Note that iNumber->GetPrecision() might be < 0
  else if (!iNumber->IsInt() && iNumber->GetPrecision() < (LispInt)digits_to_bits(aBasePrecision, BASE10))
  {
    if (iString)
    {// have string representation, can extend precision
      iNumber->SetTo(iString->c_str(),aBasePrecision, BASE10);
    }
    else
    {
  // do not have string representation, cannot extend precision!
    }
  }
  return iNumber;
}

