
#include "lispobject.h"

#ifdef YACAS_DEBUG
long theNrCurrent=0;
long theNrConstructed=0;
long theNrDestructed=0;
#endif
void IncNrObjects()
{
#ifdef YACAS_DEBUG
    theNrConstructed++;
    theNrCurrent++;
#endif
}
void DecNrObjects()
{
#ifdef YACAS_DEBUG
    theNrDestructed++;
    theNrCurrent--;
#endif
}



LispObject::LispObject()
{
//TODO remove?    iReferenceCount = 0;
#ifdef YACAS_DEBUG
    IncNrObjects();
#endif
}


LispObject::~LispObject()
{
}

LispStringPtr LispObject::String() const
{
    return NULL;
}

LispPtr* LispObject::SubList()
{
    return NULL;
}

LispPtr* LispObject::ExtraInfo()
{
    return NULL;
}


GenericClass* LispObject::Generic()
{
    return NULL;
}

EvalFuncBase* LispObject::EvalFunc()
{
    return NULL;
}

void LispObject::SetEvalFunc(EvalFuncBase* aEvalFunc)
{
    LISPASSERT(0);
}


LispInt LispObject::Equal(LispObject& aOther)
{
    // next line handles the fact that either one is a string
    if (String() != aOther.String())
        return 0;

    //So, no strings.
    LispPtr *iter1 = SubList();
    LispPtr *iter2 = aOther.SubList();
    LISPASSERT(iter1 != NULL && iter2 != NULL);

    // check all elements in sublist
    while (iter1->Get()!= NULL && iter2->Get()!=NULL)
    {
        if (! iter1->Get()->Equal(*iter2->Get() ))
            return 0;
    }
    //One list longer than the other?
    if (iter1->Get()== NULL && iter2->Get()==NULL)
        return 1;
    return 0;
}








LispPtr& LispPtr::operator=(LispPtr& aOther)
{
    Set(aOther.Get());
    return *this;
}

LispPtr& LispPtr::operator=(LispObject& aOther)
{
    Set(&aOther);
    return *this;
}


LispIterator::LispIterator(LispPtr& aPtr)
{
    iPtr = &aPtr;
}

LispIterator& LispIterator::operator=(LispPtr& aPtr)
{
    iPtr = &aPtr;
    return *this;
}
    

