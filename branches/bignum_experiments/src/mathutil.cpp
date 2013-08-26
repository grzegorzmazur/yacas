
#include "yacasprivate.h"

LispInt PlatAsciiToInt(const LispChar* aString)
{
    LispInt result=0;
    bool negative=false;

    if (*aString == '-')
    {
        negative=true;
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

