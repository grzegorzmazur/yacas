
#include "grower.h"
#include "stubs.h"
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
        while (aNrItems > iNrAllocated)
            iNrAllocated += iGranularity;
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
    int i;
    LISPASSERT(!iArrayOwnedExternally);
    LISPASSERT(aIndex+aCount<=iNrItems);

    int high=(iNrItems-aCount)*iItemSize;
    int delta = aCount*iItemSize;
    for (i=aIndex*iItemSize;i<high;i++)
    {
        iArray[i] = iArray[i+delta];
    }
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


