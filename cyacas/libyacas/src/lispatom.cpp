
#include "yacas/lispatom.h"
#include "yacas/lisperror.h"
#include "yacas/numbers.h"
#include "yacas/standard.h"

#include <algorithm>
#include <cassert>

/// construct an atom from a string representation.
LispObject* LispAtom::New(LispEnvironment& aEnvironment, const std::string& aString)
{
    if (IsNumber(aString.c_str(),true))  // check if aString is a number (int or float)
        return new LispNumber(new LispString(aString), aEnvironment.Precision());

    return new LispAtom(aEnvironment.HashTable().LookUp(aString));
}

LispAtom::LispAtom(const LispString* aString) : iString(aString)
{
    assert(aString);
    ++aString->iReferenceCount;
}

LispAtom::LispAtom(const LispAtom& other) : LispObject(other), iString(other.iString)
{
  ++iString->iReferenceCount;
}

LispAtom::~LispAtom()
{
  --iString->iReferenceCount;
}


const LispString* LispAtom::String()
{
    assert(iString);
    return iString;
}

//------------------------------------------------------------------------------
// LispSublist methods

LispSubList* LispSubList::New(LispObject* aSubList)
{
    LispSubList* self = new LispSubList(aSubList);

    if (!self)
      throw LispErrNotEnoughMemory();

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
        bool busy = (iter.getObj()->iReferenceCount == 1);
        while (busy) // while there are things to delete...
        {
      // TODO: woof -- fix this ugliness!
            LispPtr nextToDelete;
            // Make sure "next" holds the tail of the list
            nextToDelete = (iter.getObj()->Nixed());
            // Separate out the current element...
            if (iter.getObj()->iReferenceCount == 1)
            {// Destructive operation only if necessary...
                iter.getObj()->Nixed() = (nullptr);
                // And delete it.
                (*iter) = (nullptr);
            }
            else
                busy=false;
            next = (nextToDelete);
            iter = next;
            if (!iter.getObj())
                busy=false;
        }
    }
}

//------------------------------------------------------------------------------
// LispGenericClass methods

LispGenericClass* LispGenericClass::New(GenericClass* aClass)
{
    LispGenericClass* self = new LispGenericClass(aClass);

    if (!self)
      throw LispErrNotEnoughMemory();

    return self;
}

LispGenericClass::LispGenericClass(GenericClass* aClass) : iClass(aClass)
{
    assert(aClass!=nullptr);
    aClass->iReferenceCount++;
}

LispGenericClass::~LispGenericClass()
{
    if (--iClass->iReferenceCount == 0)
        delete iClass;
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
    assert(iNumber.ptr());  // either the string is null or the number but not both
    LispString *str = new LispString;
    // export the current number to string and store it as LispNumber::iString
    iNumber->ToString(*str, bits_to_digits(std::max(1,iNumber->GetPrecision()),BASE10), BASE10);
    iString = str;
  }
  return iString;
}

/// Return a BigNumber object.
// Will create a BigNumber object out of a stored string, at given precision (in decimal) - that's why the aPrecision argument must be here - but only if no BigNumber object is already present
BigNumber* LispNumber::Number(int aBasePrecision)
{
  if (!iNumber)
  {  // create and store a BigNumber out of string
    assert(iString.ptr());
    RefPtr<LispString> str;
    str = iString;
    // aBasePrecision is in digits, not in bits, ok
    iNumber = new BigNumber(str->c_str(), aBasePrecision, BASE10);
  }

  // check if the BigNumber object has enough precision, if not, extend it
  // (applies only to floats). Note that iNumber->GetPrecision() might be < 0
  else if (!iNumber->IsInt() && iNumber->GetPrecision() < (int)digits_to_bits(aBasePrecision, BASE10))
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

