/** \file lisptype.h
  * Declarations of types that could become platform-dependent
  */


#ifndef YACAS_LISPTYPE_H
#define YACAS_LISPTYPE_H

#include <stdlib.h>
#include "choices.h"

// Type definitions for the base types
typedef char LispChar;
typedef signed int LispInt;

#ifndef WIN32
#  define LISPEXPORT
#  define LISPIMPORT
#else
#  define LISPEXPORT  //__declspec(dllexport)
#  define LISPIMPORT  //__declspec(dllimport)
#endif

typedef unsigned ReferenceType;

// These define the internal types for the arbitrary precision
// number module. The larger they are the better. PlatDoubleWord
// should be at least twice as big as PlatWord, to prevent overflowing
// during multiplication.


#ifndef LINUX64
//#define SUPPORT_LONG_LONG
#  ifdef  SUPPORT_LONG_LONG
typedef unsigned long PlatWord;
typedef unsigned long long PlatDoubleWord;
typedef signed long long PlatSignedDoubleWord;
#  else
typedef unsigned short PlatWord;
typedef unsigned long PlatDoubleWord;
typedef signed long PlatSignedDoubleWord;
#  endif
#else
typedef unsigned int PlatWord;
typedef unsigned long int PlatDoubleWord;
typedef signed long int PlatSignedDoubleWord;
#endif


#endif
