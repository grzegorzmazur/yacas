
#include <stdlib.h>
#include <string.h>
#include "lisperror.h"
#include "stubs.h"


LispCharPtr PlatAlloc(LispInt aNrBytes)
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

LispCharPtr PlatReAlloc(LispCharPtr aOrig, LispInt aNrBytes)
{
    LispCharPtr result = (LispCharPtr)realloc(aOrig, aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}
void PlatFree(LispCharPtr aOrig)
{
    free(aOrig);
}


