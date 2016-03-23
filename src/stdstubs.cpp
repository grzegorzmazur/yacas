#include "yacas/stubs.h"

#include "yacas/lisperror.h"

#include <cstdlib>

void * PlatStubAlloc(LispInt aNrBytes)
{
    void * result = malloc(aNrBytes);

    if (!result)
        throw LispErrNotEnoughMemory();

    return result;
}

void * PlatStubReAlloc(void * aOrig, LispInt aNrBytes)
{
    void * result = realloc(aOrig, aNrBytes);

    if (!result)
        throw LispErrNotEnoughMemory();

    return result;
}

void PlatStubFree(void * aOrig)
{
    free(aOrig);
}


