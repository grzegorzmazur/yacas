
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


