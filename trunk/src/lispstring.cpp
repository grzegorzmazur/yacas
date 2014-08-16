
/** \file lispstring.cpp
 *  implementation of the more heavy functions that should not be inlined.
 */

#include "yacas/yacasprivate.h"
#include "yacas/lispstring.h"
#include "yacas/stubs.h"

void LispString::SetString(const LispChar * aString)
{
    const std::size_t length = std::strlen(aString);
    resize(length + 1);
    std::memcpy(data(), aString, (length + 1) * sizeof (value_type));
}

void LispString::SetStringCounted(const LispChar* aString, LispInt aLength)
{
    resize(aLength + 1);
    value_type* const aT = data();
    std::memcpy(aT, aString, aLength * sizeof (value_type));
    aT[aLength] = '\0';
}

void LispString::SetStringStringified(const LispChar* aString)
{
    const std::size_t length = std::strlen(aString);
    resize(length + 1 + 2);

    value_type* const aT = data();

    aT[0] = '\"';
    std::memcpy(aT + 1, aString, length * sizeof (value_type));
    aT[length+1] = '\"';
    aT[length+2] = '\0';
}

LispInt LispString::operator==(const LispString& aString)
{
    if (size() != aString.size())
        return 0;

    return !std::strcmp(data(), aString.data());
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

