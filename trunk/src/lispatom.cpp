
#include "yacasprivate.h"
#include "lispatom.h"
#include "lispassert.h"
#include "lisperror.h"
#include "numbers.h"
#include "standard.h"

const unsigned int BASE10 = 10;
const unsigned int BASE2 = 2;

//#ifdef YACAS_DEBUG
#include <stdio.h> //DEBUG
//#endif

/// construct an atom from a string representation.
LispObject* LispAtom::New(LispEnvironment& aEnvironment, LispStringPtr aString)
{
  LispObject* self;
#ifndef NO_USE_BIGFLOAT
  if (IsNumber(aString->String(),LispTrue))	// check if aString is a number (int or float)
  {
    /// construct a number from a decimal string representation (also create a number object)
    self = NEW LispNumber(aEnvironment.HashTable(), aString, aEnvironment.BinaryPrecision());
  }
  else
#endif
  {
    self = NEW LispAtom(aString);
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
//printf("lispatom1\n");
    iString->DecreaseRefCount();
//printf("lispatom2\n");
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
//printf("generic object just got deleted!\n");
        delete iClass;
//printf("deleted\n");
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
LispNumber::LispNumber(LispHashTable* aHashTable, BigNumber* aNumber,LispStringPtr aString)
  : iHashTable(aHashTable)
{
  iString = aString;
  iNumber = aNumber;
}


    /// construct from a BigNumber; the string representation will be absent until requested
LispNumber::LispNumber(LispHashTable& aHashTable, BigNumber* aNumber)
  : iHashTable(&aHashTable)
{
  iString = NULL;
  iNumber =aNumber;
}

    /// construct from a decimal string representation (also create a number object) and use aPrecision bits (not decimal digits!) 
LispNumber::LispNumber(LispHashTable& aHashTable, LispStringPtr aString, LispInt aPrecision)
  : iHashTable(&aHashTable)
{
  iString = aString;
  iNumber = NULL;	// purge whatever it was
  // create a new BigNumber object out of iString, set its precision in bits
  Number(aPrecision);
}

    /// return a string representation in decimal (always in decimal!)
LispStringPtr LispNumber::String() 
{
  if (iString.Ptr() == NULL)
  {
    LISPASSERT(iNumber.Ptr() != NULL);
    LISPASSERT(iHashTable != NULL);
    LispString *str = NEW LispString;
    // export the current number to string and store it as LispNumber::iString
    // FIXME API breach: precision must be in digits, not in bits here!
    iNumber->ToString(*str,iNumber->GetPrecision());
    // register the string with the hash table
    // - do we actually want this to be done? (maybe only for small numbers?)
    LISPASSERT(iHashTable != NULL);
    iString = iHashTable->LookUp(str);
//	iString = str;	// this does not work: various rules with explicit numbers fail

//#ifdef YACAS_DEBUG
//printf("Converting to string representation %s\n",iString->String()); //DEBUG
//#endif
  }
  return iString.Ptr();
}

LispNumber::~LispNumber()
{
//  delete iNumber;
  iNumber = NULL;
}
LispObject* LispNumber::Copy(LispInt aRecursed)
{
    LispObject *copied;
    copied = NEW LispNumber(iHashTable, iNumber.Ptr(), iString.Ptr());

#ifdef YACAS_DEBUG
    copied->SetFileAndLine(iFileName, iLine);
#endif
    return copied;
}

    /// create a BigNumber object out of a stored string, at given precision (in bits)
BigNumber* LispNumber::Number(LispInt aPrecision)
{
  if (iNumber.Ptr() == NULL)
  {
    LISPASSERT(iString.Ptr() != NULL);
//#ifdef YACAS_DEBUG
//printf("Converting from string representation %s\n",iString->String()); //DEBUG
//#endif
    RefPtr<LispString> str;
    str = iString.Ptr();
    // FIXME API breach: aPrecision is supposed to be in digits, not in bits!
    iNumber = NEW BigNumber(str->String(), aPrecision, BASE10);
  }
  else if (iNumber->GetPrecision() < aPrecision && !iNumber->IsInt())
  {
    if (iString.Ptr())
    {// FIXME same API breach
      iNumber->SetTo(iString.Ptr()->String(),aPrecision);
    }
    else
    {	// precision in bits, ok
      iNumber->Precision(aPrecision);
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
