
/** \file stubs.h interface to platform-dependent functions
 */

#ifndef YACAS_STUBS_H
#define YACAS_STUBS_H

#include <cstddef>

#ifdef NO_GLOBALS
  void* PlatStubAlloc(std::size_t aNrBytes);
  void* PlatStubReAlloc(void* aOrig, std::size_t aNrBytes);
  void PlatStubFree(void* aOrig);

  #define PlatAlloc PlatStubAlloc
  #define PlatReAlloc PlatStubReAlloc
  #define PlatFree PlatStubFree
#else
  void* PlatObAlloc(std::size_t nbytes);
  void PlatObFree(void *p);
  void* PlatObReAlloc(void *p, std::size_t nbytes);
  void PlatObSetThreadSafe(bool);

  #define PlatAlloc(nr)        PlatObAlloc(nr)
  #define PlatReAlloc(orig,nr) PlatObReAlloc(orig, nr)
  #define PlatFree(orig)       PlatObFree(orig)

#endif

#endif
