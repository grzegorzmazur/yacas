
#include <E32STD.H>

inline LispInt PlatStrLen(const LispCharPtr aString)
{
    LispInt nr=0;
    while ((*aString) != '\0')
    {
        aString++;                 
        nr++;
    }
    return nr;
}

inline void PlatMemCopy(LispCharPtr aTarget, LispCharPtr aSource, LispInt aNrBytes)
{
  Mem::Copy(aTarget, aSource, aNrBytes);
}

inline void PlatMemSet(LispCharPtr aTarget, LispChar aByte, LispInt aNrBytes)
{
  Mem::Fill(aTarget,aNrBytes,aByte);
}



