
#include <E32STD.H>

inline LispInt PlatStrLen(const LispChar * aString)
{
    LispInt nr=0;
    while ((*aString) != '\0')
    {
        aString++;                 
        nr++;
    }
    return nr;
}

inline LispInt StrEqual(const LispChar * ptr1, const LispChar * ptr2)
{
//    return !strcmp(ptr1,ptr2);
#if 1
    while (*ptr1 != 0 && *ptr2 != 0)
    {
        if (*ptr1++ != *ptr2++)
            return 0;
    }
    if (*ptr1 != *ptr2)
        return 0;
    return 1;
#else	// Faster?
	LispChar ch;
    while ((ch = *ptr1++) == *ptr2++) if (ch == 0) return 1;
    return 0;
#endif
}

inline void PlatMemCopy(void * aTarget, const void * aSource, LispInt aNrBytes)
{
    Mem::Copy(aTarget, aSource, aNrBytes);
}

inline void PlatMemMove(void * aTarget, const void * aSource, LispInt aNrBytes)
{
    Mem::Copy(aTarget, aSource, aNrBytes);
}

inline void PlatMemSet(void * aTarget, char aByte, LispInt aNrBytes)
{
    Mem::Fill(aTarget,aNrBytes,aByte);
}

#define StrCompare(s1,s2) strcmp((char*)s1,(char*)s2)


