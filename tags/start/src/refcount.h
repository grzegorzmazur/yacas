
#ifndef __refcount_h__
#define __refcount_h__

#include "lispassert.h"
/** Implementation of a reference-counted object. This object doesn't
 *  implement deletion, it just does the bookkeeping. The routine that
 *  calls DecreaseRefCount is responsible for actually deleting the
 *  object if this function returns zero.
 */
class RefCountedObject
{
public:
    inline RefCountedObject();
    inline void IncreaseRefCount();
    inline ReferenceType DecreaseRefCount();
    inline ReferenceType ReferenceCount() const;

private:
    ReferenceType iReferenceCount;
};

inline RefCountedObject::RefCountedObject() : iReferenceCount(0) {}

inline void RefCountedObject::IncreaseRefCount()
{
    LISPASSERT(iReferenceCount<ReferenceMax);
    iReferenceCount++;
}

inline ReferenceType RefCountedObject::DecreaseRefCount()
{
//    LISPASSERT(iReferenceCount>0);
    iReferenceCount--;
    return iReferenceCount;
}

inline ReferenceType RefCountedObject::ReferenceCount() const
{
    return iReferenceCount;
}

#endif

