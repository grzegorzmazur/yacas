
/** \file stubs.h interface to platform-dependent functions
 */

#ifndef __stubs_h__
#define __stubs_h__

#include "lisptype.h"

/** Simple function that determines if two strings are equal,
  should be defined in stubs.inl */
//inline LispInt StrEqual(LispCharPtr ptr1, LispCharPtr ptr2);


#ifdef NO_GLOBALS
  LispCharPtr PlatAlloc(LispInt aNrBytes);
  LispCharPtr PlatReAlloc(LispCharPtr aOrig, LispInt aNrBytes);
  void PlatFree(LispCharPtr aOrig);
  #define NEW new 
#else

  void *PlatObAlloc(size_t nbytes);
  void PlatObFree(void *p);
  void *PlatObReAlloc(void *p, size_t nbytes);

#ifdef YACAS_DEBUG
#include "debugmem.h"
  #define PlatAlloc(nr)        (LispCharPtr)YacasMallocPrivate((size_t)nr,__FILE__,__LINE__)
  #define PlatReAlloc(orig,nr) (LispCharPtr)YacasReAllocPrivate((void*)orig,(size_t)nr,__FILE__,__LINE__)
  #define PlatFree(orig)       YacasFreePrivate((void*)orig)
  #define NEW new (__FILE__,__LINE__)
  #define CHECKPTR(ptr) CheckPtr(ptr,__FILE__,__LINE__)
#else
  #define PlatAlloc(nr)        (LispCharPtr)PlatObAlloc((size_t)nr)
  #define PlatReAlloc(orig,nr) (LispCharPtr)PlatObReAlloc((void*)orig,(size_t)nr)
  #define PlatFree(orig)       PlatObFree((void*)orig)
  #define NEW new 
  #define CheckPtr( anAllocatedPtr, file, line )
  #define CHECKPTR(ptr)
#endif


#ifdef YACAS_DEBUG
#include <stdio.h>
#include <new>
#define NEW_THROWER throw (std::bad_alloc)
#define DELETE_THROWER throw ()
inline void* operator new(size_t size) NEW_THROWER
{
    printf("WARNING! Global new called\n");
#ifndef YACAS_DEBUG
    int* ptr = NULL;
    *ptr = 1;
#endif
    return PlatAlloc(size);
}
inline void* operator new[](size_t size) NEW_THROWER
{
    printf("WARNING! Global new called\n");
#ifndef YACAS_DEBUG
    int* ptr = NULL;
    *ptr = 1;
#endif
    return PlatAlloc(size);
}
inline void operator delete(void* object) DELETE_THROWER
{
    printf("WARNING! Global delete called\n");
#ifndef YACAS_DEBUG
    int* ptr = NULL;
    *ptr = 1;
#endif
    PlatFree((LispCharPtr)object);
}
inline void operator delete[](void* object) DELETE_THROWER
{
    printf("WARNING! Global delete called\n");
#ifndef YACAS_DEBUG
    int* ptr = NULL;
    *ptr = 1;
#endif
    PlatFree((LispCharPtr)object);
}

#endif



#endif

#include "stubs.inl"

#endif


