/** \file lisptype.h
  * Declarations of types that could become platform-dependent
  */


#ifndef __lisptype_h__
#define __lisptype_h__


// Type definitions for the base types
#define LispChar  char
#define LispInt   int
#define LispUnsLong unsigned long
#define LispShort short
#define LispCharPtr LispChar *
#define LispBoolean int

#define LISPEXPORT EXPORT_C
#define LISPIMPORT IMPORT_C

// Number of bits in long for GMP mode
#define BITS_PER_LONG 32

    
// Number of bits to use for reference-counting. This can actually
// grow significantly, when sub-expressions are copied (in which
// case a LispSubList references the same sequence of LispObjects!)
#define ReferenceBits 31
#define ReferenceType long
#define ReferenceMax ((long)0x7fffffff)

// Platform-independent booleans
#define LispFalse 0
#define LispTrue 1
    
// Dynamic link library loader class
#define DLLCLASS    LispDllBase

    
#include <E32STD.H>

// Exception handling. At the time of writing this was not yet
// implemented in the same way in several c++ compilers...
#define LispThrow(_e) User::Leave(_e)
#define LispTrap(_a,_o,_e) \
    { \
     TRAPD(err,_a); \
     if (err) \
     { \
         _e.iCleanup.Delete(); \
         _o.Write(_e.ErrorString(err)); \
        _o.Write("\n"); \
     } \
    }


#define SAFEPUSH(_env,_obj)  _env.iCleanup.Push(_obj)

#define SAFEPOP(_env)  _env.iCleanup.Pop()

#define SystemCall system

    
    
// These define the internal types for the arbitrary precision
// number module. The larger they are the better. PlatDoubleWord
// should be at least twice as big as PlatWord, to prevent overflowing
// during multiplication.

/*TODO
#define PlatWord unsigned char
#define PlatDoubleWord unsigned short
#define PlatSignedDoubleWord signed short
*/

#define PlatWord unsigned short
#define PlatDoubleWord unsigned long
#define PlatSignedDoubleWord signed long


#endif

