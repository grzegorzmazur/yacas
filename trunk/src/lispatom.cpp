
#include "yacasprivate.h"
#include "lispatom.h"
#include "lispassert.h"
#include "lisperror.h"
#include "numbers.h"
#include "standard.h"

#include <stdio.h> //DEBUG

LispObject* LispAtom::New(LispStringPtr aString)
{
  LispObject* self;
#ifndef NO_USE_BIGFLOAT
  if (IsNumber(aString->String(),LispTrue))
  {
    self = NEW LispNumber(aString);
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


LispStringPtr LispAtom::String() const
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
/*TODO remove?
    iEvalFunc = NULL;
*/
}

LispPtr* LispSubList::SubList()
{
    return &iSubList;
}

/*TODO remove?
EvalFuncBase* LispSubList::EvalFunc()
{
    return iEvalFunc;
}

void LispSubList::SetEvalFunc(EvalFuncBase* aEvalFunc)
{
    iEvalFunc = aEvalFunc;
}
*/
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


LispNumber::LispNumber(LispHashTable* aHashTable, BigNumber* aNumber,LispStringPtr aString)
  : iHashTable(aHashTable)
{
  iNumber = aNumber;
  iString = aString;
}


LispNumber::LispNumber(LispHashTable& aHashTable, BigNumber* aNumber)
  : iHashTable(&aHashTable)
{
  iString = NULL;
  iNumber =aNumber;
}
LispNumber::LispNumber(LispStringPtr aString)
  : iHashTable(NULL)
{
  iString = aString;
  iNumber = NULL;
  Number(10);
}
LispStringPtr LispNumber::String() const
{
  if (iString == NULL)
  {
    LISPASSERT(iNumber.Ptr() != NULL);
    LISPASSERT(iHashTable != NULL);
    LispStringPtr str = NEW LispString;
    iNumber->ToString(*str,iNumber->GetPrecision());
    ((LispNumber*)this)->iString = iHashTable->LookUp(str);

#ifdef YACAS_DEBUG
printf("Converting to string representation %s\n",iString->String()); //DEBUG
#endif
  }
  return iString;
}

LispNumber::~LispNumber()
{
//  delete iNumber;
  iNumber = NULL;
}
LispObject* LispNumber::Copy(LispInt aRecursed)
{
    LispObject *copied;
    copied = NEW LispNumber(iHashTable, iNumber.Ptr(), iString);

#ifdef YACAS_DEBUG
    copied->SetFileAndLine(iFileName, iLine);
#endif
    return copied;
}
BigNumber* LispNumber::Number(LispInt aPrecision)
{
  if (iNumber.Ptr() == NULL)
  {
    LISPASSERT(iString != NULL);
#ifdef YACAS_DEBUG
printf("Converting from string representation %s\n",iString->String()); //DEBUG
#endif
    iNumber = NEW BigNumber(iString->String(),aPrecision);
  }
  else if (iNumber->GetPrecision() < aPrecision)
  {
    iNumber = NEW BigNumber(iString->String(),aPrecision);
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
