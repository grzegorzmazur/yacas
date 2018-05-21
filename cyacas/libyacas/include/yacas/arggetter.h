#ifndef YACAS_ARGGETTER_H
#define YACAS_ARGGETTER_H

#include "lispenvironment.h"

/// Get an argument that should be a short integer
int GetShortIntegerArgument(LispEnvironment& aEnvironment, int aStackTop, int iArgNr);

#endif
