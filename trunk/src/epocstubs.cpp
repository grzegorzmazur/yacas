
#include "lisperror.h"
#include "stubs.h"



LispCharPtr PlatAlloc(LispInt aNrBytes)
{
    LispCharPtr result = (LispCharPtr)User::Alloc(aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}

LispCharPtr PlatReAlloc(LispCharPtr aOrig, LispInt aNrBytes)
{
    LispCharPtr result = (LispCharPtr)User::ReAlloc(aOrig, aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}


