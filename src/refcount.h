
#ifndef __refcount_h__
#define __refcount_h__

#include "lispassert.h"
#include "yacasbase.h"
/** Implementation of a reference-counted object. This object doesn't
 *  implement deletion, it just does the bookkeeping. The routine that
 *  calls DecreaseRefCount is responsible for actually deleting the
 *  object if this function returns zero.
 */
class RefCountedObjectBase
{
public:
    inline RefCountedObjectBase();
    inline void IncreaseRefCount();
    inline ReferenceType DecreaseRefCount();
    inline ReferenceType ReferenceCount() const;

private:
    ReferenceType iReferenceCount;
};

class RefCountedObject : public RefCountedObjectBase, public YacasBase
{
};

inline RefCountedObjectBase::RefCountedObjectBase() : iReferenceCount(0) {}

inline void RefCountedObjectBase::IncreaseRefCount()
{
    LISPASSERT(iReferenceCount<ReferenceMax);
    iReferenceCount++;
}

inline ReferenceType RefCountedObjectBase::DecreaseRefCount()
{
//    LISPASSERT(iReferenceCount>0);
    iReferenceCount--;
    return iReferenceCount;
}

inline ReferenceType RefCountedObjectBase::ReferenceCount() const
{
    return iReferenceCount;
}

#endif

