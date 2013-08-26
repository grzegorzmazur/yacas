
#ifndef __arggetter_h__
#define __arggetter_h__

#include "yacasbase.h"

/// Get an argument that should be a short integer
LispInt GetShortIntegerArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt iArgNr);

#define ShortIntegerArgument(_i,_argnr) LispInt _i = GetShortIntegerArgument(aEnvironment,aStackTop,_argnr)


#endif

