
#include "yacas/yacasprivate.h"
#include "yacas/lispenvironment.h"
#include "yacas/lispstring.h"
#include "yacas/standard.h"
#include "yacas/errors.h"
#include "yacas/lispeval.h"
#include "yacas/arggetter.h"
#include "yacas/platmath.h"
#include "yacas/genericstructs.h"
#include "yacas/errors.h"

#define ARGUMENT(i)  aEnvironment.iStack.GetElement(aStackTop+i)

static
LispString * GetIntegerArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  LispString * str = ARGUMENT(aArgNr)->String();
  CHK_ARG_CORE(str,aArgNr);
  CHK_ARG_CORE(IsNumber(str->c_str(),false),aArgNr);
  return str;
}

LispInt GetShortIntegerArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  LispString * str = GetIntegerArgument(aEnvironment, aStackTop, aArgNr);
  return InternalAsciiToInt(str);
}

