
#include "lispatom.h"
#include "lispassert.h"
#include "lisperror.h"


LispAtom* LispAtom::New(LispStringPtr aString)
{
    LispAtom* self = new LispAtom(aString);
    Check(self!=NULL,KLispErrNotEnoughMemory);
    return self;
}

LispAtom::LispAtom(LispStringPtr aString)
{
    LISPASSERT(aString!=NULL);
    iString = aString;
    aString->IncreaseRefCount();
}
LispAtom::~LispAtom()
{
//printf("lispatom1\n");
    iString->DecreaseRefCount();
//printf("lispatom2\n");
}


LispStringPtr LispAtom::String() const
{
    return iString;
}

LispObject* LispAtom::Copy(LispInt aRecursed)
{
    LispObject *copied = new LispAtom(iString);
    return copied;
}


LispObject* LispAtom::SetExtraInfo(LispPtr& aData)
{
    LispObject* result = new LispAnnotatedObject<LispAtom>(this);
    result->SetExtraInfo(aData);
    return result;
}



LispSubList* LispSubList::New(LispObject* aSubList)
{
    LispSubList* self = new LispSubList(aSubList);
    Check(self!=NULL,KLispErrNotEnoughMemory);
    return self;
}

LispSubList::LispSubList(LispObject* aSubList)
{
    iSubList.Set(aSubList);
    iEvalFunc = NULL;
}

LispPtr* LispSubList::SubList()
{
    return &iSubList;
}

EvalFuncBase* LispSubList::EvalFunc()
{
    return iEvalFunc;
}

void LispSubList::SetEvalFunc(EvalFuncBase* aEvalFunc)
{
    iEvalFunc = aEvalFunc;
}

LispObject* LispSubList::Copy(LispInt aRecursed)
{
    //TODO recursed copy needs to be implemented still
    LISPASSERT(aRecursed == 0);
    LispObject *copied = new LispSubList(iSubList.Get());
    return copied;
}


LispObject* LispSubList::SetExtraInfo(LispPtr& aData)
{
    LispObject* result = new LispAnnotatedObject<LispSubList>(this);
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
    LispGenericClass* self = new LispGenericClass(aClass);
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
    LispObject *copied = new LispGenericClass(iClass);
    return copied;
}

LispObject* LispGenericClass::SetExtraInfo(LispPtr& aData)
{
    LispObject* result = new LispAnnotatedObject<LispGenericClass>(this);
    result->SetExtraInfo(aData);
    return result;
}


