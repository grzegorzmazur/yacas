
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

#define InternalEval iEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)



LispStringPtr GetIntegerArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  LispStringPtr str;
  str = ARGUMENT(aArgNr).Get()->String();
  CHK_ARG_CORE(str != NULL,aArgNr);
  CHK_ARG_CORE(IsNumber(str->String(),LispFalse),aArgNr);
  return str;
}

void* GetVoidStruct(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr, LispCharPtr aTypeString)
{
  GenericClass *gen = ARGUMENT(aArgNr).Get()->Generic();
  CHK_ARG_CORE(gen != NULL ,aArgNr);
  CHK_ARG_CORE(StrEqual(gen->TypeName(),aTypeString),aArgNr);
  GenericStruct *str = (GenericStruct*)gen;
  return str->Data();
}

LispInt GetShortIntegerArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  LispStringPtr str = GetIntegerArgument(aEnvironment, aStackTop, aArgNr);
  return InternalAsciiToInt(str->String());
}

LispStringPtr GetStringArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  LispStringPtr str;
  str = ARGUMENT(aArgNr).Get()->String();
  CHK_ARG_CORE(InternalIsString(str),aArgNr);
  return aEnvironment.HashTable().LookUpUnStringify(str->String());
}



LispStringPtr GetAtomArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  LispStringPtr str;
  str = ARGUMENT(aArgNr).Get()->String();
  CHK_ARG_CORE(str != NULL,aArgNr);
  return aEnvironment.HashTable().LookUp(str->String());
}




void GetListArgument(LispPtr& aResult, LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  aResult.Set(ARGUMENT(aArgNr).Get());
  CHK_ARG_CORE(aResult.Get() != NULL,aArgNr);
  CHK_ARG_CORE(aResult.Get()->SubList() != NULL, aArgNr);
}


void ReturnShortInteger(LispEnvironment& aEnvironment, LispPtr& aResult, LispInt r)
{
  char s[100];
  InternalIntToAscii(s,r);
  aResult.Set(LispAtom::New(aEnvironment,s));
}

void SetShortIntegerConstant(LispEnvironment& aEnvironment,
                                    LispCharPtr aName,
                                    LispInt aValue)
{
  LispPtr value;
  ReturnShortInteger(aEnvironment,value, aValue);
  aEnvironment.SetVariable( aEnvironment.HashTable().LookUp(aName),value);
}
double GetDoubleFloatArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  return GetDouble(ARGUMENT(aArgNr).Get());
}

void ReturnDoubleFloat(LispEnvironment& aEnvironment, LispPtr& aResult, double r)
{
  aResult.Set(Double(aEnvironment,r));
}

void ReturnVoidStruct(LispEnvironment& aEnvironment,
                      LispPtr& aResult,
                      LispCharPtr aName,
                      void* aData,
                      void (*aFree)(void*))
{
    aResult.Set(LispGenericClass::New(NEW GenericStruct(aName,aData,aFree)));
}



