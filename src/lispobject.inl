
#include "lispassert.h"
#include "lisperror.h"


inline void LispPtr::DestroyPrevious()
{
  if (iNext != NULL)
  {
    if (!iNext->DecreaseRefCount())
    {
      delete iNext;
#ifdef YACAS_DEBUG
      DecNrObjects();
#endif
    }
  }
}


inline void LispPtr::DoSet(LispObject* aNext)
{
  iNext = aNext;
  if (iNext != NULL)
    iNext->IncreaseRefCount();
}

inline void LispPtr::Set(LispObject* aNext)
{
  // first increase the reference count of the new object, because if we
  // are setting a LispPtr to itself, we don't want to prematurely delete
  // the object.
  if (aNext != NULL)
    aNext->IncreaseRefCount();
  if (iNext != NULL)
  {
    if (!iNext->DecreaseRefCount())
    {
      delete iNext;
#ifdef YACAS_DEBUG
      DecNrObjects();
#endif
    }
  }
  iNext = aNext;
}


inline LispPtr& LispObject::Next()
{
    return iNext;
}

inline LispInt LispObject::operator==(LispObject& aOther)
{
    return Equal(aOther);
}

inline LispInt LispObject::operator!=(LispObject& aOther)
{
    return !Equal(aOther);
}

inline LispPtr::LispPtr()
{
    iNext = NULL;
}

inline LispPtr::LispPtr(LispPtr& aOther)
{
    DoSet(aOther.Get());
}

inline LispPtr::LispPtr(LispObject& aOther)
{
    DoSet(&aOther);
}

inline LispPtr::~LispPtr()
{
    DestroyPrevious();
}


inline LispObject* LispPtr::Get() const
{
    return iNext;
}


inline LispObject* LispPtr::operator()()
{
    return iNext;
}

inline LispPtr* LispIterator::Ptr()
{
    return iPtr;
}

inline void LispPtr::GoNext()
{
    LISPASSERT(iNext != NULL);
    Set(iNext->Next().Get());
}



inline LispObject* LispIterator::operator()()
{
    return iPtr->Get();
}

inline void LispIterator::GoNext()
{
    Check(iPtr->Get() != NULL,KLispErrListNotLongEnough);
    iPtr = &(iPtr->Get()->Next());
}

inline void LispIterator::GoSub()
{
    Check(iPtr->Get() != NULL,KLispErrInvalidArg);
    Check(iPtr->Get()->SubList() != NULL,KLispErrNotList);
    iPtr = iPtr->Get()->SubList();
}




inline LispInt LispPtrArray::Size()
{
    return iSize;
}

inline LispPtr& LispPtrArray::GetElement(LispInt aItem)
{
    LISPASSERT(aItem>=0 && aItem<iSize);
    return iArray[aItem];
}

inline void LispPtrArray::SetElement(LispInt aItem,LispObject* aObject)
{
    LISPASSERT(aItem>=0 && aItem<iSize);
    iArray[aItem].Set(aObject);
}

