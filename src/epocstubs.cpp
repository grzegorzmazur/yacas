
#include "yacasprivate.h"
#include "lisperror.h"
#include "stubs.h"


#ifndef SIZE_T
#define SIZE_T unsigned int
#endif

void* PlatStubAlloc(SIZE_T aNrBytes);
void *PlatStubReAlloc(void *aOrig, SIZE_T aNrBytes);
void PlatStubFree(void *aOrig);


void* PlatStubAlloc(SIZE_T aNrBytes)
{
    LispChar * result = (LispChar *)User::Alloc(aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}

void *PlatStubReAlloc(void *aOrig, SIZE_T aNrBytes)
{
    LispChar * result = (LispChar *)User::ReAlloc(aOrig, aNrBytes);
    Check(result!=NULL,KLispErrNotEnoughMemory);
    return result;
}
void PlatStubFree(void *aOrig)
{
  User::Free(aOrig);
}

