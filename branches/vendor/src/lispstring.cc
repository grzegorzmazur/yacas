
/** \file lispstring.cpp
 *  implementation of the more heavy functions that should not be inlined.
 */

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



LispInt StrEqual(LispCharPtr ptr1, LispCharPtr ptr2)
{
    while (*ptr1 != 0 && *ptr2 != 0)
    {
        if (*ptr1++ != *ptr2++)
            return 0;
    }
    if (*ptr1 != *ptr2)
        return 0;
    return 1;
}

LispInt LispString::operator==(const LispString& aString)
{
    if (NrItems() != aString.NrItems())
        return 0;

    LispCharPtr ptr1 = (LispCharPtr)iArray;
    LispCharPtr ptr2 = (LispCharPtr)aString.iArray;
    return StrEqual(ptr1,ptr2);
}



