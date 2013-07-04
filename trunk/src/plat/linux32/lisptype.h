/** \file lisptype.h
  * Declarations of types that could become platform-dependent
  */


#ifndef __lisptype_h__
#define __lisptype_h__

#include <stdlib.h>
#include "../../choices.h"

// Type definitions for the base types
#define LispChar  char
#define LispInt   signed int
#define LispUnsLong unsigned long

#define LISPEXPORT
#define LISPIMPORT

// Number of bits to use for reference-counting. This can actually
// grow significantly, when sub-expressions are copied (in which
// case a LispSubList references the same sequence of LispObjects!)
#ifdef USE_LONG_REF_COUNTS
#define ReferenceBits 32
#define ReferenceType unsigned
#define ReferenceMax ((unsigned)0xffffffff)
#else
#define ReferenceBits 16
#define ReferenceType unsigned short
#define ReferenceMax ((unsigned short)0xffff)
#endif


#define SAFEPUSH(_env,_obj) // _env.iCleanup.Push(_obj)
#define SAFEPOP(_env) // _env.iCleanup.Pop()


// These define the internal types for the arbitrary precision
// number module. The larger they are the better. PlatDoubleWord
// should be at least twice as big as PlatWord, to prevent overflowing
// during multiplication.



//#define SUPPORT_LONG_LONG
#ifdef  SUPPORT_LONG_LONG
#  define PlatWord unsigned long
#  define PlatDoubleWord unsigned long long
#  define PlatSignedDoubleWord signed long long
#else
#  define PlatWord unsigned short
#  define PlatDoubleWord unsigned long
#  define PlatSignedDoubleWord signed long
#endif



#endif

