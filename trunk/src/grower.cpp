
#include "yacasprivate.h"
#include "grower.h"
#include "lispassert.h"



CArrayGrowerBase::~CArrayGrowerBase() 
{ 
    if (!iArrayOwnedExternally)
    {
        PlatFree(iArray);
    }
}

void CArrayGrowerBase::Clear()
{
    LISPASSERT(!iArrayOwnedExternally);
    PlatMemSet(iArray,0,iNrItems*iItemSize);
}

int CArrayGrowerBase::BaseAppend(LispChar* aValue)
{
    LISPASSERT(!iArrayOwnedExternally);
  GrowTo(iNrItems+1);
  PlatMemCopy(BaseItem(iNrItems-1),aValue,iItemSize);
  return iNrItems-1;
}

void CArrayGrowerBase::SetGranularity(LispInt aGranularity)
{
    LISPASSERT(aGranularity>0);
    iGranularity=aGranularity;
}


void CArrayGrowerBase::GrowTo(int aNrItems)
{
    LISPASSERT(!iArrayOwnedExternally);
    if (aNrItems > iNrAllocated)
    {
        /*
         if (iNrAllocated == 0)
            iNrAllocated = 1;
        while (aNrItems > iNrAllocated)
            iNrAllocated *= 2;
        */
        /**/

        iNrAllocated = iGranularity*((aNrItems+iGranularity-1)/iGranularity);
//         while (aNrItems > iNrAllocated)
//             iNrAllocated += iGranularity;
        /**/
        if (iArray == NULL)
        {
            iArray = (LispChar*)PlatAlloc(iNrAllocated*iItemSize);
        }
        else
        {
            iArray = (LispChar*)PlatReAlloc(iArray,iNrAllocated*iItemSize);
        }
    }
    SetNrItems(aNrItems);
}

void CArrayGrowerBase::Delete(int aIndex, int aCount)
{
    LISPASSERT(!iArrayOwnedExternally);
    LISPASSERT(aIndex+aCount<=iNrItems);

    PlatMemMove(&iArray[aIndex*iItemSize],
                &iArray[(aIndex+aCount)*iItemSize],
                (iNrItems-aCount-aIndex)*iItemSize
               );

    SetNrItems(iNrItems-aCount);
}

void CArrayGrowerBase::MoveBlock(int aSrcIndex, int aTrgIndex)
{
    int i;
    LISPASSERT(!iArrayOwnedExternally);
    if (aSrcIndex > aTrgIndex)
    {
        for (i=(aSrcIndex-1)*iItemSize;i>=aTrgIndex*iItemSize;i--)
        {
            iArray[i+iItemSize] = iArray[i];
        }
    }
    else if (aSrcIndex < aTrgIndex)
    {
        for (i=aSrcIndex*iItemSize;i<aTrgIndex*iItemSize;i++)
        {
            iArray[i] = iArray[i+iItemSize];
        }
    }
}


