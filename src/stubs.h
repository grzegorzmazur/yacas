
/** \file stubs.h interface to platform-dependent functions
 */

#ifndef __stubs_h__
#define __stubs_h__

#include "lisptype.h"

LispCharPtr PlatAlloc(LispInt aNrBytes);
LispCharPtr PlatReAlloc(LispCharPtr aOrig, LispInt aNrBytes);
void PlatFree(LispCharPtr aOrig);

void* operator new(unsigned long size);
void operator delete(void* object);

inline void* operator new(unsigned long size)
{
    return PlatAlloc(size);
}
inline void* operator new[](unsigned long size)
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

#include "stubs.inl"

#endif


