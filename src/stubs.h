
/** \file stubs.h interface to platform-dependent functions
 */

#ifndef __stubs_h__
#define __stubs_h__

#ifndef __yacasprivate_h__
#error "You might not want to include this file outside of Yacas!"
#endif

#include "lisptype.h"

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


enum TAllocTypes
{
    EAlloc=0
};

inline void* operator new(size_t size)
{
    return PlatAlloc(size);
}
inline void* operator new[](size_t size)
{
    return PlatAlloc(size);
}
inline void operator delete(void* object)
{
    PlatFree((LispCharPtr)object);
}
inline void operator delete[](void* object)
{
    PlatFree((LispCharPtr)object);
}

#endif

#include "stubs.inl"

#endif


