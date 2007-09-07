/** \file lisptype.h

  * Declarations of types that could become platform-dependent

  */

#ifndef __lisptype_h__
#define __lisptype_h__

#include <stdlib.h>
#include "../../choices.h"

// Type definitions for the base types

#define LispChar  char
#define LispInt   int
#define LispUnsLong unsigned long
#define LispShort short
#define LispBoolean int

#define LISPEXPORT  //__declspec(dllexport)
#define LISPIMPORT  //__declspec(dllimport)

// Number of bits in long
#define BITS_PER_LONG 32

// Number of bits to use for reference-counting. This can actually
// grow significantly, when sub-expressions are copied (in which
// case a LispSubList references the same sequence of LispObjects!)
#ifdef USE_LONG_REF_COUNTS
#define ReferenceBits 32
#define ReferenceType unsigned long
#define ReferenceMax ((unsigned long)0xffffffff)
#else
#define ReferenceBits 16
#define ReferenceType unsigned short
#define ReferenceMax ((unsigned short)0xffff)
#endif

// Platform-independent booleans
#define LispFalse 0
#define LispTrue 1

#define EXE_DLL_PLUGINS // Enable plugins inside the exe by default

// System calls
#define SystemCall system

// Exception handling. At the time of writing this was not yet

// implemented in the same way in several c++ compilers...

#define LispThrow(_e) throw(_e)

#define LispTrap(_a,_o,_e) \
  try \
    { \
       _a; \
    } \
    catch(LispInt b) \
    { \
     if (_e.ErrorString(b)[0] != '\0') \
     {\
      if (_e.iInputStatus.LineNumber()>=0) \
      { \
          LispChar linenum[20]; \
          InternalIntToAscii(linenum,_e.iInputStatus.LineNumber()); \
          _o.Write(_e.iInputStatus.FileName());\
          _o.Write("(");\
          _o.Write(linenum);\
          _o.Write(") : ");\
        } \
        _e.iCleanup.Delete(); \
        _o.Write(_e.ErrorString(b)); \
        _o.Write("\n"); \
      }\
    }

#define SAFEPUSH(_env,_obj) // _env.iCleanup.Push(_obj)
#define SAFEPOP(_env) // _env.iCleanup.Pop()

// These define the internal types for the arbitrary precision
// number module. The larger they are the better. PlatDoubleWord
// should be at least twice as big as PlatWord, to prevent overflowing
// during multiplication.

#define PlatWord unsigned short
#define PlatDoubleWord unsigned long
#define PlatSignedDoubleWord signed long

// Fake config.h, some defines I'm sure are valid on Windows
#define HAVE_MATH_H   1
#define HAVE_STDIO_H  1
#define HAVE_STDARG_H 1
#define HAVE_STRTOD   1



#endif



