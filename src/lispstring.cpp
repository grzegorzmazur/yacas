
/** \file lispstring.cpp
 *  implementation of the more heavy functions that should not be inlined.
 */

#include "yacasprivate.h"
#include "lispstring.h"
#include "stubs.h"


void LispString::SetString(LispCharPtr aString,
                           LispBoolean aStringOwnedExternally)
{
    LispInt length = PlatStrLen(aString);  // my own strlen

    if (aStringOwnedExternally)
    {
        SetExternalArray(aString, length+1);
    }
    else
    {
        GrowTo(length+1);
        LispInt i;
        for (i=0;i<=length;i++)
        {
            Item(i) = aString[i];
        }
    }
}
void LispString::SetStringCounted(LispCharPtr aString,LispInt aLength)
{
    GrowTo(aLength+1);
    LispInt i;
    for (i=0;i<aLength;i++)
    {
        Item(i) = aString[i];
    }
    Item(aLength) = '\0';
}
void LispString::SetStringStringified(LispCharPtr aString)
{
    LispInt length = PlatStrLen(aString);  // my own strlen

    GrowTo(length+1 + 2);
    LispInt i;
    Item(0) = '\"';
    for (i=0;i<length;i++)
    {
        Item(i+1) = aString[i];
    }
    Item(length+1) = '\"';
    Item(length+2) = '\0';
}

void LispString::SetStringUnStringified(LispCharPtr aString)
{
    LispInt length = PlatStrLen(aString);  // my own strlen

    GrowTo(length-1);
    LispInt i;
    for (i=1;i<length-1;i++)
    {
        Item(i-1) = aString[i];
    }
    Item(length-2) = '\0';
}


LispInt LispString::operator==(const LispString& aString)
{
    if (NrItems() != aString.NrItems())
        return 0;

    LispCharPtr ptr1 = (LispCharPtr)iArray;
    LispCharPtr ptr2 = (LispCharPtr)aString.iArray;
    return StrEqual(ptr1,ptr2);
}

LispString::~LispString()
{
    LISPASSERT(ReferenceCount() == 0);
}


void LispStringSmartPtr::Set(LispStringPtr aString)
{
    if (iString)
    {
//        printf("refcount was %d\n",iString->ReferenceCount());
        iString->DecreaseRefCount();
        if (iString->ReferenceCount() == 0)
            delete iString;
//        printf("refcount becomes %d\n",iString->ReferenceCount());
    }
    iString = aString;
    if (iString)
    {
//        printf("refcount was %d\n",iString->ReferenceCount());
        iString->IncreaseRefCount();
//        printf("refcount becomes %d\n",iString->ReferenceCount());
    }
}
LispStringSmartPtr::~LispStringSmartPtr()
{
    Set(NULL);
}



