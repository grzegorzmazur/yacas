

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

inline void PlatMemCopy(LispCharPtr aTarget, LispCharPtr aSource, LispInt aNrBytes)
{
    memcpy(aTarget, aSource, aNrBytes);
}


inline void PlatMemSet(LispCharPtr aTarget, LispChar aByte, LispInt aNrBytes)
{
    memset(aTarget, aByte, aNrBytes);
}



