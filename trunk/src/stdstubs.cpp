
#include <stdlib.h>
#include <string.h>
#include "yacasprivate.h"
#include "lisperror.h"
#include "stubs.h"

void * PlatStubAlloc(LispInt aNrBytes)
{
    void * result = malloc(aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}

void * PlatStubReAlloc(void * aOrig, LispInt aNrBytes)
{
    void * result = realloc(aOrig, aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}

void PlatStubFree(void * aOrig)
{
    free(aOrig);
}


