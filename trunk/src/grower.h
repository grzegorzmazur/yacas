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

/** \class CArrayGrowerBase has the base utility functions needed for a
 *  growing array. It implements the operations without knowing the types
 *  the array stores. A derived template class can then use these utility
 *  functions, so the inline functions from the template class stay small.
 */
class CArrayGrowerBase
{
public:
    virtual ~CArrayGrowerBase();
    /// Return the number of items currently in the array
    inline int NrItems() const {return iNrItems;}
    /** Set the current number of items. Make sure the allocated array
     * is big enough! This routine doesn't do that for you. Use GrowTo
     * to make sure the allocated array is large enough.
     */
    inline void SetNrItems(int aNrItems);
    /** Change the number of elements in the array, growing the allocated
     *  array if needed.
     */
    void GrowTo(int aNrItems);
    /// Delete element(s) from the array
    void Delete(int aIndex, int aCount=1);

    void MoveBlock(int aSrcIndex, int aTrgIndex);
    int BaseAppend(LispChar* aValue);
    void Clear();

    /** see if the array is owned externally. If so, the outside world
     * is responsible for freeing the array. This class can then also
     * not modify the size of the array (using append, delete, or growto
     * are prohibited). This can greatly speed up operations if the
     * array can be shared with some other piece of code.
     */
    inline LispBoolean ArrayOwnedExternally();
    /// Set the step size for growing the array
    void SetGranularity(LispInt aGranularity);
    /// Return the number of allocated cells.
    inline LispInt NrAllocated() {return iNrAllocated; }; //For debugging purposes only!
protected:
    inline CArrayGrowerBase(int aItemSize, int aGranularity);
    inline LispChar* BaseItem(int aIndex);

protected:
    LispInt iItemSize;
    LispInt iNrItems;
    LispChar* iArray;
    LispInt iNrAllocated;
    LispInt iGranularity;
    LispBoolean iArrayOwnedExternally;
};

/** template class useful for implementing a dynamic growing array
 *  for any arbitrary type of object. If using the array to maintain
 *  objects, please use pointers to the objects, and use the
 * CDeletingArrayGrower to automatically delete the objects at destruction.
 *
 */
template <class T>
class CArrayGrower : public CArrayGrowerBase
{
public:
    /** ElementType can be used outside this class as the type of the
     * object in the array. This is useful in templated functions that
     * work on a CArrayGrower without being part of CArrayGrower
     */
    typedef T ElementType;
    typedef T* ElementTypePtr;
    
    CArrayGrower(int aGranularity=8);
    /// Access to an element in the array
    inline T& operator[](const int aIndex) const;
    inline T& Item(const int aIndex) const;

    /// Append an element to an array
    inline int Append(T aValue);

    inline void Move(int aSrcIndex, int aTrgIndex);
    /// Insert object aObj, aCount times.
    inline void Insert(int aIndex, T& aObj, LispInt aCount=1);
    /** Set the array to an external array. This means the array will
     * not be freed at destruction time
     */
    inline void SetExternalArray(T* aArray, LispInt aNrItems);
    
    /// Copy the array to another array
    inline void CopyToExternalArray(T*& aArray, LispBoolean aPreAlloc);
};

/** \class CDeletingArrayGrower calls delete on each element in the
 * array at destruction time. This is useful if the array is a list
 * of pointers to objects.
 */
template <class T>
class CDeletingArrayGrower : public CArrayGrower<T>
{
public:
    CDeletingArrayGrower(LispInt aGranularity=8);
    ~CDeletingArrayGrower();
};




#include "grower.inl"


#endif


