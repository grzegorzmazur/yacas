
/** \file stubs.h interface to platform-dependent functions
 */

#ifndef __stubs_h__
#define __stubs_h__

#include "lisptype.h"

/** Simple function that determines if two strings are equal */
inline LispInt StrEqual(LispCharPtr ptr1, LispCharPtr ptr2);


#ifdef NO_GLOBALS
LispCharPtr PlatAlloc(LispInt aNrBytes);
LispCharPtr PlatReAlloc(LispCharPtr aOrig, LispInt aNrBytes);
void PlatFree(LispCharPtr aOrig);
#else
void *PlatObAlloc(size_t nbytes);
void PlatObFree(void *p);
void *PlatObReAlloc(void *p, size_t nbytes);
#define PlatAlloc(nr)        (LispCharPtr)PlatObAlloc((size_t)nr)
#define PlatReAlloc(orig,nr) (LispCharPtr)PlatObReAlloc((void*)orig,(size_t)nr)
#define PlatFree(orig)       PlatObFree((void*)orig)

#ifdef YACAS_DEBUG
#include <stdio.h>
inline void* operator new(size_t size)
{
    int* ptr = NULL;
    *ptr = 1;
    printf("new called\n");
    return PlatAlloc(size);
}
inline void* operator new[](size_t size)
{
    int* ptr = NULL;
    *ptr = 1;
    printf("new called\n");
    return PlatAlloc(size);
}
inline void operator delete(void* object)
{
    int* ptr = NULL;
    *ptr = 1;
    printf("delete called\n");
    PlatFree((LispCharPtr)object);
}
inline void operator delete[](void* object)
{
    int* ptr = NULL;
    *ptr = 1;
    printf("delete called\n");
    PlatFree((LispCharPtr)object);
}

#endif


#endif

#include "stubs.inl"

#endif


