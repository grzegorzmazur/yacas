

#include <stdlib.h>
#include <string.h>

inline LispInt PlatStrLen(const LispChar * aString)
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

inline LispInt StrEqual(const LispChar * ptr1, const LispChar * ptr2)
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


inline void PlatMemCopy(void * aTarget, const void * aSource, LispInt aNrBytes)
{
    memcpy(aTarget, aSource, aNrBytes);
}
inline void PlatMemMove(void * aTarget, const void * aSource, LispInt aNrBytes)
{
    memmove(aTarget, aSource, aNrBytes);
}


inline void PlatMemSet(LispChar * aTarget, LispChar aByte, LispInt aNrBytes)
{
    memset(aTarget, aByte, aNrBytes);
}

#define StrCompare(s1,s2) strcmp((char*)s1,(char*)s2)


