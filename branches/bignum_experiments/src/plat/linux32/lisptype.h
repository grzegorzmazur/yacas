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
#define LispShort short
#define LispBoolean int

#define LISPEXPORT
#define LISPIMPORT

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

// System calls
#define SystemCall system

// Exception handling. At the time of writing this was not yet
// implemented in the same way in several c++ compilers...

#ifdef NO_EXCEPTIONS
  #define LispThrow(_e) throw(_e)
  #define LispTrap(_u,_a,_o,_e) _a
#else
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

//            CHECKPTR(_e.iInputStatus.FileName());
//            _o.Write("Error on line ");
//            _o.Write(linenum);
//            _o.Write(" in file [");
//            _o.Write(_e.iInputStatus.FileName());
//            _o.Write("]\n");


// The following line should also be printed. BUT: it causes
// a segfault on following commands!
//            _o.Write(_e.iInputStatus.Line(_e.iInputDirectories));
//            _o.Write("]\nLine error occurred on:\n>>>");
//      _o.Write("\n");
//

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

