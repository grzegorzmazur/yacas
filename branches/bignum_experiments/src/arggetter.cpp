
#include "yacasprivate.h"
#include "lispenvironment.h"
#include "lispstring.h"
#include "standard.h"
#include "errors.h"
#include "lispeval.h"
#include "arggetter.h"
#include "platmath.h"
#include "genericstructs.h"
#include "errors.h"

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

