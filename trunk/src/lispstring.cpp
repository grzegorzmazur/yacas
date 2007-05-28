
/** \file lispstring.cpp
 *  implementation of the more heavy functions that should not be inlined.
 */

#include "yacasprivate.h"
#include "lispstring.h"
#include "stubs.h"

void LispString::SetString(const LispChar * aString)
{
  LispInt length = PlatStrLen(aString);  // my own strlen
  ResizeTo(length+1);
  ElementType * aT = elements();
  for (LispInt i = 0; i <= length; i++)
  {
    aT[i] = aString[i];
  }
}

void LispString::SetStringCounted(const LispChar * aString,LispInt aLength)
{
  ResizeTo(aLength+1);
  ElementType * aT = elements();
  for (LispInt i = 0; i < aLength; i++)
  {
    aT[i] = aString[i];
  }
  aT[aLength] = '\0';
}

void LispString::SetStringStringified(const LispChar * aString)
{
  LispInt length = PlatStrLen(aString);  // my own strlen
  ResizeTo(length+1 + 2);
  ElementType * aT = elements();
  aT[0] = '\"';
  for (LispInt i = 0; i < length; i++)
  {
    aT[i+1] = aString[i];
  }
  aT[length+1] = '\"';
  aT[length+2] = '\0';
}

void LispString::SetStringUnStringified(const LispChar * aString)
{
  LispInt length = PlatStrLen(aString);  // my own strlen
  ResizeTo(length-1);
  ElementType * aT = elements();
  for (LispInt i = 1; i < length-1; i++)
  {
    aT[i-1] = aString[i];
  }
  aT[length-2] = '\0';
}

LispInt LispString::operator==(const LispString& aString)
{
  if (Size() != aString.Size())
    return 0;
  const LispChar * ptr1 = elements();
  const LispChar * ptr2 = aString.elements();
  return StrEqual(ptr1,ptr2);
}

LispString::~LispString()
{
}

LispStringSmartPtr& LispStringSmartPtr::operator=(LispString * aString)
{
  // Increment first.
  if (aString)
    ++aString->iReferenceCount;
  if (iString)
  {
    --iString->iReferenceCount;
    if (iString->iReferenceCount == 0) delete iString;
  }
  iString = aString;
  return *this;
}

LispStringSmartPtr::~LispStringSmartPtr()
{
  if (iString && !--iString->iReferenceCount)
    delete iString;
}

