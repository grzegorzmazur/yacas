
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
#define RESULT       aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i)  aEnvironment.iStack.GetElement(aStackTop+i)


LispString * GetIntegerArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  LispString * str = ARGUMENT(aArgNr)->String();
  CHK_ARG_CORE(str,aArgNr);
  CHK_ARG_CORE(IsNumber(str->c_str(),LispFalse),aArgNr);
  return str;
}

void* GetVoidStruct(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr, LispChar * aTypeString)
{
  GenericClass *gen = ARGUMENT(aArgNr)->Generic();
  DYNCAST(GenericStruct,aTypeString,str,gen)
  CHK_ARG_CORE(str,aArgNr);
  return str->Data();
}

LispInt GetShortIntegerArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  LispString * str = GetIntegerArgument(aEnvironment, aStackTop, aArgNr);
  return InternalAsciiToInt(str);
}

LispString * GetStringArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  LispString * str = ARGUMENT(aArgNr)->String();
  CHK_ARG_CORE(InternalIsString(str),aArgNr);
  return aEnvironment.HashTable().LookUpUnStringify(str->c_str());
}



LispString * GetAtomArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  LispString * str = ARGUMENT(aArgNr)->String();
  CHK_ARG_CORE(str,aArgNr);
  return aEnvironment.HashTable().LookUp(str->c_str());
}




void GetListArgument(LispPtr& aResult, LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  aResult = (ARGUMENT(aArgNr));
  CHK_ARG_CORE(aResult,aArgNr);
  CHK_ARG_CORE(aResult->SubList(), aArgNr);
}


void ReturnShortInteger(LispEnvironment& aEnvironment, LispPtr& aResult, LispInt r)
{
  char s[100];
  InternalIntToAscii(s,r);
  aResult = (LispAtom::New(aEnvironment,s));
}

void SetShortIntegerConstant(LispEnvironment& aEnvironment,
                                    LispChar * aName,
                                    LispInt aValue)
{
  LispPtr value;
  ReturnShortInteger(aEnvironment,value, aValue);
  aEnvironment.SetVariable( aEnvironment.HashTable().LookUp(aName),value,LispFalse);
}
double GetDoubleFloatArgument(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aArgNr)
{
  return GetDouble(ARGUMENT(aArgNr));
}

void ReturnDoubleFloat(LispEnvironment& aEnvironment, LispPtr& aResult, double r)
{
  aResult = (Double(aEnvironment,r));
}

void ReturnVoidStruct(LispEnvironment& aEnvironment,
                      LispPtr& aResult,
                      LispChar * aName,
                      void* aData,
                      void (*aFree)(void*))
{
    aResult = (LispGenericClass::New(NEW GenericStruct(aName,aData,aFree)));
}



