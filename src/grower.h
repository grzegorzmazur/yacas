/** \file grower.h Template class that implements dynamic growing arrays.
 *  Yacas implements its own growing array class to ensure correct behaviour
 *  on all platforms (stl was still in development and not supported on all
 *  platforms when development started).
 *
 * grower started 1998 ayal Pinkus
 *
 */
 
#ifndef _GROWER_H_
#define _GROWER_H_

#include "yacasbase.h"
#include "lispassert.h"



// Default placement versions of operator new and delete, placed here because I do not want to include <new>
inline void* operator new(size_t, void* __p) throw() { return __p; }
inline void* operator new[](size_t, void* __p) throw() { return __p; }
inline void  operator delete  (void*, void*) throw() { };
inline void  operator delete[](void*, void*) throw() { };


template <class T>
inline void constructAt(void * to_obj, const T * from_obj)
{
	new (to_obj) T(*from_obj);
}

class ArrOps
{
public:
	virtual bool isPOD() const { return false; }
	virtual void construct(void * buffer) const = 0;
	virtual void construct(void * to_obj, const void * from_obj) const = 0;
	virtual void destruct(void * obj) const = 0;
	virtual int granularity() const { return 8; }
	static void resize(char** pArray, const ArrOps& opers, int& iSize, int& iCapacity, int aSize, int aItemSize, int ext);
    static void remove(char** pArray, const ArrOps& opers, int& iSize, int aIndex, int aCount, int aItemSize);
	static void reserve(char** pArray, int& iCapacity, int aSize, int aItemSize);
  //static void* insert(char** pArray, const ArrOps& opers, int& iSize, void* aData, int aIndex, int aCount, int aSize, int aExtSize);
};

template <class T>
class ArrOpsCustomObj : public ArrOps
{
public:
	ArrOpsCustomObj() {}
	inline void construct(void * buffer) const
	{ new (buffer) T; }
	inline void construct(void * to_obj, const void * from_obj) const
	{ constructAt(to_obj, static_cast<const T*>(from_obj)); }
	inline void destruct(void * obj) const
	{ /*obj;*/ static_cast<T*>(obj)->~T(); }
	//inline int size() const { return sizeof(T); }
};

template <class T>
class ArrOpsPOD : public ArrOps
{
public:
	ArrOpsPOD() {}
	inline bool isPOD() const { return true; }
	inline void construct(void * buffer) const {}
	inline void construct(void * to_obj, const void * from_obj) const
	{ *static_cast<T*>(to_obj) = *static_cast<const T*>(from_obj); }
	inline void destruct(void * obj) const {}
	//inline int size() const { return sizeof(T); }
};

template <class T>
class ArrOpsCustomPtr : public ArrOps
{
	typedef T* TY;
public:
	ArrOpsCustomPtr() {}
	inline void construct(void * buffer) const
	{ *static_cast<TY*>(buffer) = 0; }
	inline void construct(void * to_obj, const void * from_obj) const
	{ *static_cast<TY*>(to_obj) = *static_cast<const TY*>(from_obj); }
	inline void destruct(void * obj) const {}
	//inline int size() const { return sizeof(TY); }
	inline int granularity() const { return 8; }
};

template <class T>
class ArrOpsDeletingObj {};

template <class T>
class ArrOpsDeletingPtr : public ArrOpsCustomPtr<T>
{
	typedef T* TY;
public:
	inline void destruct(void * obj) const
	{ delete *static_cast<const TY*>(obj); }
};

/** template class useful for implementing a dynamic growing array
 *  for any arbitrary type of object. If using the array to maintain
 *  objects, please use pointers to the objects, and use the
 * CDeletingArrayGrower to automatically delete the objects at destruction.
 */
template <class T, class TOps >
class CArrayGrower : public YacasBase
{
public:
    /** ElementType can be used outside this class as the type of the
     * object in the array. This is useful in templated functions that
     * work on a CArrayGrower without being part of CArrayGrower
     */
	typedef T value_type;	// almost the same, but T must be non-const

	typedef LispInt size_type;
	typedef T ElementType;

    CArrayGrower()
		: iArray(0)
		, iSize(0)
		, iCapacity(0)
		, iArrayOwnedExternally(LispFalse)
    {
    }
	virtual ~CArrayGrower()
	{
		Clear();
	}
	inline void Clear()	// oddly, only *one* use outside destructor!?
	{
		if (iSize)
		{
			const TOps opers; if(!opers.isPOD())
			while (iSize) opers.destruct(iArray + --iSize);
		}
		if (!iArrayOwnedExternally) PlatFree(iArray);
		iArray = 0;
		iCapacity = iSize = 0;
		iArrayOwnedExternally = LispFalse;
	}
    inline size_type Size() const { return iSize; }
	inline void Resize(size_type aSize)
	{
		LISPASSERT(aSize<=iCapacity);
		iSize = aSize;
		// TODO: woof woof -- destruct or construct as necessary?
		// How does this differ from GrowTo?
	}
protected:
	void moreCapacity(size_type aSize, int g)	// almost independent of T
	{
		// Compute a new iCapacity >= aSize, with iCapacity % g == 0.
		// We assume g is a power of 2.  (fwiw, in two's-complement, ~(g-1) == -g.
		iCapacity = (aSize + g) & ~(g-1);
		if (!iArray)
			iArray = (value_type*)PlatAlloc(iCapacity*sizeof(value_type));
		else
			// we assume 'memcpy' suffices for moving the existing items.
			iArray = (value_type*)PlatReAlloc(iArray,iCapacity*sizeof(value_type));
	}
public:
	inline void GrowTo(size_type aSize)
	{
		LISPASSERT(!iArrayOwnedExternally);
//void ArrOps::resize(char** pArray, const ArrOps& opers, int& iSize, int& iCapacity, int aSize, int aItemSize, int ext)
		TOps opers;
		if (aSize > iCapacity) moreCapacity(aSize, opers.granularity());
		if (!opers.isPOD())
		{
			if (iSize < aSize)
				for (int ii = iSize; ii < aSize; ii++)
					opers.construct(iArray + ii);
			else
				for (int ii = aSize; ii < iSize; ii++)
					opers.destruct(iArray + ii);
		}
		iSize = aSize;
	}
    void Delete(size_type aIndex, size_type aCount=1)
	{
		LISPASSERT(aIndex>=0 && aIndex<iSize);
		// TODO: woof -- in-line this?
		ArrOps::remove((char**)&iArray, TOps(), iSize, aIndex, aCount, sizeof(value_type));
	}
	inline LispBoolean ArrayOwnedExternally()
	{
		return iArrayOwnedExternally;
	}
public:
    /// Access to an element in the array
	/*
    inline value_type& At(const size_type aIndex) const
    {
		LISPASSERT(aIndex>=0 && aIndex<iSize);
		return iArray[aIndex];
    }
	*/
    inline value_type& operator[](const size_type aIndex) const
    {
		return iArray[aIndex];
    }

    /// Append an element to an array
	template <class Type>
    inline void Append(const Type& aVal)
    {
		if (iSize >= iCapacity) moreCapacity(iSize+1, TOps().granularity());
		new ((void *)(iArray+iSize)) value_type(aVal);
		++iSize;
	}

	// If s > t, then circular shift right:
	//    x[t ... s] <-- { x[s], x[t ... s-1] }
	// If s < t, then circular shift left:
	//    x[s ... t] <-- { x[s+1 ... t], x[s] }
    inline void Mooove(size_type aSrcIndex, size_type aTrgIndex)
    {
	    LISPASSERT(!iArrayOwnedExternally);
		LISPASSERT(aSrcIndex>=0 && aSrcIndex<iSize && aTrgIndex>=0 && aTrgIndex<iSize );
		if (aSrcIndex > aTrgIndex)
			rotate(iArray[aTrgIndex], iArray[aTrgIndex+1], iArray[aSrcIndex+1]);
		else
			rotate(iArray[aSrcIndex], iArray[aTrgIndex-1], iArray[aTrgIndex+1]);
    }

    /// Insert object aObj at aIndex, aCount times.
    inline void Insert(size_type aIndex, const value_type& aObj, size_type aCount=1)
	{
		const size_type oldItems = iSize;
		LISPASSERT(aIndex <= oldItems && aCount >= 0);
		GrowTo(iSize+aCount);
		value_type * pOld = iArray+oldItems;
		value_type * pEnd = iArray+iSize;
		int i = iSize - aIndex;	// = (oldItems - aIndex) + aCount
		for ( ; i > aCount; i--)
			*--pEnd = *--pOld;
		while (i-- > 0)
			*--pEnd = aObj;
	}

    /** Set the array to an external array. This means the array will
     * not be freed at destruction time
     */
    inline void SetExternalArray(value_type* aArray, size_type aSize)
	{
		LISPASSERT(!iArray || iArrayOwnedExternally == LispTrue);
		iArray = aArray;
		iSize = aSize;
		iArrayOwnedExternally = LispTrue;
	  //iCapacity = 0;	// TODO: what are implications that this is missing?
	}

    /// Copy the array to another array
    inline void CopyToExternalArray(value_type * aArray)
	{
		PlatMemCopy(aArray,iArray,iSize*sizeof(value_type));
	}

protected:
	// TODO: should we remove, and just make iArray protected?
	inline value_type * elements() const { return iArray; }
private:
    value_type * iArray;
  //size_type iItemSize;
    size_type iSize;
    size_type iCapacity;
  //size_type iGranularity;
    LispBoolean iArrayOwnedExternally;
};

/** \class CDeletingArrayGrower calls delete on each element in the
 * array at destruction time. This is useful if the array is a list
 * of pointers to objects.
 */

template <class T, class TOps >
class CDeletingArrayGrower : public CArrayGrower<T, TOps >
{
public:
	CDeletingArrayGrower() {}
	//~CDeletingArrayGrower() {}
};

#endif


