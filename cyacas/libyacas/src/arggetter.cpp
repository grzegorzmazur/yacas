
#include "yacas/lispenvironment.h"
#include "yacas/lispstring.h"
#include "yacas/standard.h"
#include "yacas/errors.h"
#include "yacas/lispeval.h"
#include "yacas/arggetter.h"
#include "yacas/platmath.h"
#include "yacas/errors.h"

static
const LispString* GetIntegerArgument(LispEnvironment& aEnvironment, int aStackTop, int aArgNr)
{
  const LispString* str = aEnvironment.iStack[aStackTop + aArgNr]->String();
  CheckArg(str, aArgNr, aEnvironment, aStackTop);
  CheckArg(IsNumber(str->c_str(),false), aArgNr, aEnvironment, aStackTop);
  return str;
}

int GetShortIntegerArgument(LispEnvironment& aEnvironment, int aStackTop, int aArgNr)
{
  const LispString* str = GetIntegerArgument(aEnvironment, aStackTop, aArgNr);
  return InternalAsciiToInt(*str);
}

