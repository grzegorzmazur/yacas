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
#define LispCharPtr LispChar *
#define LispBoolean int

#define LISPEXPORT	__declspec(dllexport)
#define LISPIMPORT	__declspec(dllimport)

// Number of bits to use for reference-counting. This can actually
// grow significantly, when sub-expressions are copied (in which
// case a LispSubList references the same sequence of LispObjects!)
#define ReferenceBits 16
#define ReferenceType unsigned short
#define ReferenceMax ((unsigned short)0xffff)


// Platform-independent booleans
#define LispFalse 0
#define LispTrue 1

// Dynamic link library loader class
#define DLLCLASS    Win32Dll
    
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
        if (_e.iInputStatus.LineNumber()>=0) \
        { \
            LispChar linenum[20]; \
            InternalIntToAscii(linenum,_e.iInputStatus.LineNumber()); \
            _o.Write("Error on line ");\
            _o.Write(linenum);\
            _o.Write(" in file [");\
            _o.Write(_e.iInputStatus.FileName()); \
			_o.Write("]\nLine error occurred on:\n"); \
            _o.Write(_e.iInputStatus.Line(_e.iInputDirectories)); \
			_o.Write("\n\n"); \
        } \
        _e.iCleanup.Delete(); \
        _o.Write(_e.ErrorString(b)); \
        _o.Write("\n"); \
    }

#define SAFEPUSH(_env,_obj) // _env.iCleanup.Push(_obj)
#define SAFEPOP(_env) // _env.iCleanup.Pop()


// These define the internal types for the arbitrary precision
// number module. The larger they are the better. PlatDoubleWord
// should be at least twice as big as PlatWord, to prevent overflowing
// during multiplication.

/*TODO
#define PlatWord unsigned char
#define PlatDoubleWord unsigned short
#define PlatSignedDoubleWord signed short
*/

/*TODO*/
#define PlatWord unsigned short
#define PlatDoubleWord unsigned long
#define PlatSignedDoubleWord signed long
/**/



#endif

