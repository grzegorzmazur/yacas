
/** \file stubs.h interface to platform-dependent functions
 */

#ifndef YACAS_STUBS_H
#define YACAS_STUBS_H

#include "lisptype.h"

#ifdef NO_GLOBALS
  void * PlatStubAlloc(LispInt aNrBytes);
  void * PlatStubReAlloc(void * aOrig, LispInt aNrBytes);
  void PlatStubFree(void * aOrig);

  #define PlatAlloc PlatStubAlloc
  #define PlatReAlloc PlatStubReAlloc
  #define PlatFree PlatStubFree
  #define NEW new
#else  // NO_GLOBALS -- goes almost to EOF
  void *PlatObAlloc(size_t nbytes);
  void PlatObFree(void *p);
  void *PlatObReAlloc(void *p, size_t nbytes);
  void PlatObSetThreadSafe(bool);
#ifdef YACAS_DEBUG
  #include "debugmem.h"
  #define PlatAlloc(nr)        YacasMallocPrivate((size_t)nr,__FILE__,__LINE__)
  #define PlatReAlloc(orig,nr) YacasReAllocPrivate(orig,(size_t)nr,__FILE__,__LINE__)
  #define PlatFree(orig)       YacasFreePrivate(orig)
  #define NEW new (__FILE__,__LINE__)
#else  // YACAS_DEBUG
  #define PlatAlloc(nr)        PlatObAlloc((size_t)nr)
  #define PlatReAlloc(orig,nr) PlatObReAlloc(orig,(size_t)nr)
  #define PlatFree(orig)       PlatObFree(orig)
  #define NEW new
#endif  // YACAS_DEBUG

#ifdef YACAS_DEBUG  // goes almost to EOF

/* Operators new and delete are only defined globally here in debug mode. This is because
 * the global new and delete operators should not be used. So the debug version makes sure
 * the executable crashes if one of these are called.
 */

#define NEW_THROWER throw ()//(std::bad_alloc)
#define DELETE_THROWER throw ()

// TODO: why doesn't MSC itself have this problem? Perhaps wrong signature of these new and delete operators?
#if defined(_MSC_VER) && _MSC_VER <= 1310  // getting C4290 warnings?  slowly increase number.
#pragma warning( disable : 4290 )  // C4290: C++ exception specification ignored except to indicate a function is not __declspec(nothrow)
#endif

void* operator new(size_t size) NEW_THROWER;
void* operator new[](size_t size) NEW_THROWER;
void operator delete(void* object) DELETE_THROWER;
void operator delete[](void* object) DELETE_THROWER;

#endif  // YACAS_DEBUG

#endif  // NO_GLOBALS

#endif
