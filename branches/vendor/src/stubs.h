
/** \file stubs.h interface to platform-dependent functions
 */

#ifndef __stubs_h__
#define __stubs_h__

#include "lisptype.h"

LispCharPtr PlatAlloc(LispInt aNrBytes);
LispCharPtr PlatReAlloc(LispCharPtr aOrig, LispInt aNrBytes);

#include "stubs.inl"

#endif


