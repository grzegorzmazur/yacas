#include <stdlib.h>
#include <string.h>

inline LispInt PlatStrLen(const LispCharPtr aString)
{
    return strlen(aString);
    /*Generic version
     LispInt nr=0;
    while ((*aString) != '\0')
    {
        aString++;                 
        nr++;
    }
    return nr;
    */
}

inline LispInt StrEqual(LispCharPtr ptr1, LispCharPtr ptr2)
{
    return !strcmp(ptr1,ptr2);
/*
    while (*ptr1 != 0 && *ptr2 != 0)
    {
        if (*ptr1++ != *ptr2++)
            return 0;
    }
    if (*ptr1 != *ptr2)
        return 0;
    return 1;
*/
}

inline void PlatMemCopy(LispCharPtr aTarget, LispCharPtr aSource, LispInt aNrBytes)
{
    memcpy(aTarget, aSource, aNrBytes);
}
inline void PlatMemMove(LispCharPtr aTarget, LispCharPtr aSource, LispInt aNrBytes)
{
    memmove(aTarget, aSource, aNrBytes);
}


inline void PlatMemSet(LispCharPtr aTarget, LispChar aByte, LispInt aNrBytes)
{
    memset(aTarget, aByte, aNrBytes);
}



