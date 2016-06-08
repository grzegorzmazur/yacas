#include "yacas/stubs.h"

#include "yacas/lisperror.h"

#include <cstdlib>

void* PlatStubAlloc(std::size_t n)
{
    void* result = malloc(n);

    if (!result)
        throw LispErrNotEnoughMemory();

    return result;
}

void* PlatStubReAlloc(void *p, std::size_t n)
{
    void* result = realloc(p, n);

    if (!result)
        throw LispErrNotEnoughMemory();

    return result;
}

void PlatStubFree(void* p)
{
    free(p);
}


