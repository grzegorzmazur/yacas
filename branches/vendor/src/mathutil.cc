
#include "lisptype.h"

LispInt PlatAsciiToInt(LispCharPtr aString)
{
    LispInt result=0;
    LispBoolean negative=LispFalse;

    if (*aString == '-')
    {
        negative=LispTrue;
        aString++;
    }
    while (*aString)
    {
        result*=10;
        result+=( (*aString) - '0');
        aString++;
    }
    if (negative)
        result=-result;
    return result;
}

