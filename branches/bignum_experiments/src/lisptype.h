/** \file lisptype.h
  * Declarations of types that could become platform-dependent
  */


#ifndef __lisptype_h__
#define __lisptype_h__

#include <stdlib.h>
#include "choices.h"

// Type definitions for the base types
#define LispChar  char
#define LispInt   signed int
#define LispUnsLong unsigned long

#ifndef WIN32
#  define LISPEXPORT
#  define LISPIMPORT
#else
#  define LISPEXPORT  //__declspec(dllexport)
#  define LISPIMPORT  //__declspec(dllimport)
#endif

#ifndef LINUX64
#  define ReferenceType unsigned
#  define ReferenceMax ((unsigned)0xffffffff)
#else
#  define ReferenceType unsigned
#  define ReferenceMax ((unsigned)0xffffffffffffffff)
#endif

#define SAFEPUSH(_env,_obj) // _env.iCleanup.Push(_obj)
#define SAFEPOP(_env) // _env.iCleanup.Pop()


// These define the internal types for the arbitrary precision
// number module. The larger they are the better. PlatDoubleWord
// should be at least twice as big as PlatWord, to prevent overflowing
// during multiplication.


#ifndef LINUX64
//#define SUPPORT_LONG_LONG
#  ifdef  SUPPORT_LONG_LONG
#    define PlatWord unsigned long
#    define PlatDoubleWord unsigned long long
#    define PlatSignedDoubleWord signed long long
#  else
#    define PlatWord unsigned short
#    define PlatDoubleWord unsigned long
#    define PlatSignedDoubleWord signed long
#  endif
#else
#  define PlatWord unsigned int
#  define PlatDoubleWord unsigned long int
#  define PlatSignedDoubleWord signed long int
#endif


#endif
