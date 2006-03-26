
#include "lispassert.h"
#include "lisperror.h"

inline LispPtr& LispObject::Nixed()
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

//------------------------------------------------------------------------------
// LispPtrArray methods

#if HAS_NEW_LispPtrArray == 0
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
    iArray[aItem] = (aObject);
}
#endif
