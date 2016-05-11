
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
#else
  void *PlatObAlloc(size_t nbytes);
  void PlatObFree(void *p);
  void *PlatObReAlloc(void *p, size_t nbytes);
  void PlatObSetThreadSafe(bool);

  #define PlatAlloc(nr)        PlatObAlloc((size_t)nr)
  #define PlatReAlloc(orig,nr) PlatObReAlloc(orig,(size_t)nr)
  #define PlatFree(orig)       PlatObFree(orig)

#endif

#endif
