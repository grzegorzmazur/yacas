
#include <stdlib.h>
#include <string.h>
#include "lisperror.h"
#include "stubs.h"


LispCharPtr PlatAlloc(LispInt aNrBytes)
{
    LispCharPtr result = (LispCharPtr)malloc(aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}

LispCharPtr PlatReAlloc(LispCharPtr aOrig, LispInt aNrBytes)
{
    LispCharPtr result = (LispCharPtr)realloc(aOrig, aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}


