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

#define LISPEXPORT
#define LISPIMPORT

#define ReferenceType unsigned
#define ReferenceMax ((unsigned)0xffffffffffffffff)


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

