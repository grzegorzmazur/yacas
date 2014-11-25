
#include <stdlib.h>
#include <string.h>
#include "yacas/yacasprivate.h"
#include "yacas/lisperror.h"
#include "yacas/stubs.h"

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


