
#include <stdlib.h>
#include <string.h>
#include "yacasprivate.h"
#include "lisperror.h"
#include "stubs.h"

/* If we have no globals, don't compile in obmalloc, but use the
 standard allocators */
#ifdef NO_GLOBALS
#define PlatStubAlloc PlatAlloc
#define PlatStubReAlloc PlatReAlloc
#define PlatStubFree PlatFree
#endif

void * PlatStubAlloc(LispInt aNrBytes)
{
    /*Test code: report on memory usage
     static int nrsmall=0,nrlarge=0;
    if (aNrBytes < 256)
        nrsmall++;
    else
        nrlarge++;
    printf("%d, %d\n",nrsmall,nrlarge);
    */
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


