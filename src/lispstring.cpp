
/** \file lispstring.cpp
 *  implementation of the more heavy functions that should not be inlined.
 */

#include "yacasprivate.h"
#include "lispstring.h"
#include "stubs.h"

void LispString::SetString(const LispChar * aString)
{
    const std::size_t length = std::strlen(aString);
    ResizeTo(length + 1);
    std::memcpy(elements(), aString, (length + 1) * sizeof (ElementType));
}

void LispString::SetStringCounted(const LispChar* aString, LispInt aLength)
{
    ResizeTo(aLength + 1);
    ElementType* const aT = elements();
    std::memcpy(aT, aString, aLength * sizeof (ElementType));
    aT[aLength] = '\0';
}

void LispString::SetStringStringified(const LispChar* aString)
{
    const std::size_t length = std::strlen(aString);
    ResizeTo(length + 1 + 2);

    ElementType* const aT = elements();

    aT[0] = '\"';
    std::memcpy(aT + 1, aString, length * sizeof (ElementType));
    aT[length+1] = '\"';
    aT[length+2] = '\0';
}

void LispString::SetStringUnStringified(const LispChar * aString)
{
    const std::size_t length = std::strlen(aString);
    ResizeTo(length - 1);
    ElementType* const aT = elements();
    std::memcpy(aT, aString + 1, (length - 1) * sizeof (ElementType));
    aT[length-2] = '\0';
}

LispInt LispString::operator==(const LispString& aString)
{
    if (Size() != aString.Size())
        return 0;

    return !std::strcmp(elements(), aString.elements());
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

