
//#include <stdlib.h>
#include "lispassert.h"

inline CArrayGrowerBase::CArrayGrowerBase(int aItemSize, int aGranularity) 
    : iItemSize(aItemSize),iNrItems(0),
      iArray(NULL),iNrAllocated(0) ,iGranularity(aGranularity),
      iArrayOwnedExternally(LispFalse)
    {
    }


inline void CArrayGrowerBase::SetNrItems(int aNrItems)
{
    LISPASSERT(aNrItems<=iNrAllocated);
    iNrItems = aNrItems;
}

inline LispChar* CArrayGrowerBase::BaseItem(int aIndex)
{
    LISPASSERT(aIndex>=0);
    LISPASSERT(aIndex<iNrItems);
    return iArray + aIndex*iItemSize;
}


inline LispBoolean CArrayGrowerBase::ArrayOwnedExternally()
{
    return iArrayOwnedExternally;
}

template <class T> 
inline CArrayGrower<T>::CArrayGrower(int aGranularity)
    :CArrayGrowerBase(sizeof(T),aGranularity) 
    { 
    }

template <class T> 
inline T& CArrayGrower<T>::Item(const int aIndex) const
    {
    LISPASSERT(aIndex>=0);
    LISPASSERT(aIndex<iNrItems);
    return ((T*)iArray)[aIndex];
    }

template <class T> 
inline T& CArrayGrower<T>::operator[](const int aIndex) const
    {
    return Item(aIndex);
    }


template <class T> 
inline int CArrayGrower<T>::Append(T aValue)
    {
    return BaseAppend((LispChar*)&aValue);
    }

template <class T> 
inline void CArrayGrower<T>::Move(int aSrcIndex, int aTrgIndex)
    {
    T toMove = Item(aSrcIndex);
    MoveBlock(aSrcIndex, aTrgIndex);
    Item(aTrgIndex) = toMove;
    }

#ifdef __yacasprivate_h__
template<class T>
inline void DestructArray(CArrayGrower<T>& aArray)
{
  int i,nr;
  nr = aArray.NrItems();
  for (i=0;i<nr;i++)
  {
    delete aArray[i];
    aArray[i] = NULL;
  }
}
#endif

template<class T>
inline void CArrayGrower<T>::Insert(int aIndex, T& aObj, LispInt aCount)
{
    GrowTo(iNrItems+aCount);
    int i;

    for (i=iNrItems-aCount-1;i>=aIndex;i--)
    {
        Item(i+aCount) = Item(i);
    }
    for (i=0;i<aCount;i++)
        Item(aIndex+i) = aObj;
}

template<class T>
inline void CArrayGrower<T>::SetExternalArray(T* aArray, LispInt aNrItems)
{
    LISPASSERT(iArray == NULL || iArrayOwnedExternally == LispTrue);
    iArray = (LispCharPtr) aArray;
    iNrItems = aNrItems;
    iArrayOwnedExternally = LispTrue;
}


#ifdef __yacasprivate_h__
template<class T>
inline void CArrayGrower<T>::CopyToExternalArray(T*& aArray, LispBoolean aPreAlloc)
{
    if (!aPreAlloc)
    {
        aArray = (T *) PlatAlloc(iNrItems*iItemSize);
    }
    PlatMemCopy((LispCharPtr) aArray,iArray,iNrItems*iItemSize);
}

template <class T>
inline CDeletingArrayGrower<T>::CDeletingArrayGrower(LispInt aGranularity)
:CArrayGrower<T>(aGranularity)
{
}

template <class T>
inline CDeletingArrayGrower<T>::~CDeletingArrayGrower()
{
  LispInt i,nr;
  nr = CDeletingArrayGrower::NrItems();
  for (i=0;i<nr;i++)
  {
    delete CDeletingArrayGrower::Item(i);
  }
}
#endif


