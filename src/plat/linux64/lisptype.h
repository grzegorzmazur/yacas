/** \file lisptype.h
  * Declarations of types that could become platform-dependent
  */


#ifndef __lisptype_h__
#define __lisptype_h__

#include <stdlib.h>
#include "../../choices.h"

// Type definitions for the base types
#define LispChar  char
#define LispInt   signed long int
#define LispUnsLong unsigned long int
#define LispShort short int

#define LISPEXPORT
#define LISPIMPORT

// Number of bits to use for reference-counting. This can actually
// grow significantly, when sub-expressions are copied (in which
// case a LispSubList references the same sequence of LispObjects!)
#define ReferenceBits 64
#define ReferenceType unsigned int
#define ReferenceMax ((unsigned int)0xffffffffffffffff)


#define SAFEPUSH(_env,_obj) // _env.iCleanup.Push(_obj)
#define SAFEPOP(_env) // _env.iCleanup.Pop()


// These define the internal types for the arbitrary precision
// number module. The larger they are the better. PlatDoubleWord
// should be at least twice as big as PlatWord, to prevent overflowing
// during multiplication.

// 64-bit version of these int types.
#define PlatWord unsigned int
#define PlatDoubleWord unsigned long int
#define PlatSignedDoubleWord signed long int

#endif

