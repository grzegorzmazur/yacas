
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

LispCharPtr PlatStubAlloc(LispInt aNrBytes)
{
    /*Test code: report on memory usage
     static int nrsmall=0,nrlarge=0;
    if (aNrBytes < 256)
        nrsmall++;
    else
        nrlarge++;
    printf("%d, %d\n",nrsmall,nrlarge);
    */
    LispCharPtr result = (LispCharPtr)malloc(aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}

LispCharPtr PlatStubReAlloc(LispCharPtr aOrig, LispInt aNrBytes)
{
    LispCharPtr result = (LispCharPtr)realloc(aOrig, aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}
void PlatStubFree(LispCharPtr aOrig)
{
    free(aOrig);
}


