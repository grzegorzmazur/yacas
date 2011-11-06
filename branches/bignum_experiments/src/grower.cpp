
#include "yacasprivate.h"
#include "grower.h"
#include "lispassert.h"

/*static*/
void ArrOps::reserve(char** pArray, int& iCapacity, int aSize, int aItemSize)
{
  if (aSize > iCapacity)
  {
      if (!*pArray)
          *pArray = (char*)PlatAlloc(aSize*aItemSize);
    else
      // we assume 'memcpy' suffices for moving the existing items.
      *pArray = (char*)PlatReAlloc(*pArray,aSize*aItemSize);
    iCapacity = aSize;
    }
}

/*static*/
void ArrOps::resize(char** pArray, const ArrOps& opers, int& iSize, int& iCapacity, int aSize, int aItemSize, int ext)
{
    if (aSize == iSize) return;
  if (aSize > iCapacity)
  {
      if (!*pArray)
          *pArray = (char*)PlatAlloc(aSize*aItemSize);
    else
      // we assume 'memcpy' suffices for moving the existing items.
      *pArray = (char*)PlatReAlloc(*pArray,aSize*aItemSize);
    iCapacity = aSize;
    }
    if (!opers.isPOD())
    {
    if (iSize < aSize)
      for (int ii = iSize; ii < aSize; ii++)
        opers.construct(*pArray + ii*aItemSize);
    else
      for (int ii = aSize; ii < iSize; ii++)
        opers.destruct(*pArray + ii*aItemSize);
  }
    iSize = aSize;
}

/*static*/
void ArrOps::remove(char** pArray, const ArrOps& opers, int& iSize, int aIndex, int aCount, int aItemSize)
{
  LISPASSERT(aIndex >= 0 && aCount >= 0 && aIndex+aCount <= iSize);
  if (!opers.isPOD())
    for (int ii = aIndex; ii < aIndex+aCount; ii++)
      opers.destruct(*pArray + ii*aItemSize);
    // we assume 'memcpy' suffices for moving the existing items.
    PlatMemMove(*pArray + aIndex*aItemSize,
                *pArray + (aIndex+aCount)*aItemSize,
                (iSize-aCount-aIndex)*aItemSize);
  iSize -= aCount;
}
