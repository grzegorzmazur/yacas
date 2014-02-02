
#ifndef YACAS_ARGGETTER_H
#define YACAS_ARGGETTER_H

#include "yacasbase.h"

/// Get an argument that should be a short integer
LispInt GetShortIntegerArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt iArgNr);

#define ShortIntegerArgument(_i,_argnr) LispInt _i = GetShortIntegerArgument(aEnvironment,aStackTop,_argnr)


#endif

