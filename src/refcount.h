
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

template<class T>
class RefPtr
{
public:
  inline RefPtr();
  inline RefPtr(T* aObject);
  inline RefPtr(RefPtr<T>& aObject);
  inline RefPtr<T>& operator=(T* aObject);
  inline RefPtr& operator=(RefPtr<T>& aObject);
  inline ~RefPtr() {SetTo(NULL);}
  inline T* operator->() const {return iObject;}
  
  inline T* Ptr() const {return iObject;}
private:
  void SetTo(T* aOther);
  T* iObject;
};

template<class T>
inline RefPtr<T>::RefPtr()
{
  iObject = NULL;
}

template<class T>
inline RefPtr<T>::RefPtr(RefPtr<T>& aObject)
{
  iObject = NULL;
  SetTo(aObject.Ptr());
}

template<class T>
inline RefPtr<T>::RefPtr(T* aObject)
{
  iObject = NULL;
  SetTo(aObject);
}

template<class T>
inline RefPtr<T>& RefPtr<T>::operator=(T* aObject)
{
  SetTo(aObject);
  return *this;
}

template<class T>
inline RefPtr<T>& RefPtr<T>::operator=(RefPtr<T>& aObject) 
{
  SetTo(aObject.Ptr());
  return *this;
}

template<class T>
inline void RefPtr<T>::SetTo(T* aOther)
{
  if (iObject)
  {
    if (!iObject->DecreaseRefCount())
    {
      delete iObject;
    }
  }
  iObject = aOther;
  if (iObject) iObject->IncreaseRefCount();
}



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

