
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
/*TODO remove
  LispStringPtr str = GetAtomArgument(aEnvironment, aStackTop, aArgNr);
  return GetDouble(str->String());
*/
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












#if 0 // TODO remove
void LispArgGetter::GoNext()
{
    iIter.GoNext();
    if (iIter() == NULL)
    {
        ErrorNrArgs(iNrArgsNeeded, iNrArgsParsed, iArguments,
                    iEnvironment);
    }
    iNrArgsParsed++;
}

LispStringPtr LispArgGetter::GetIntegerArgument(LispInt aEvaluate)
{
    GoNext();

    LispStringPtr str;

    if (aEvaluate)
    {
        LispPtr result;
        InternalEval(iEnvironment, result, *iIter.Ptr());
        str = result.Get()->String();
    }
    else
    {
        str = iIter()->String();
    }
    CheckArgType(str != NULL ,iNrArgsParsed, iArguments,iEnvironment,
                 KLispErrNotInteger);
    CheckArgType(IsNumber(str->String(),LispFalse) ,iNrArgsParsed, iArguments,iEnvironment,
                 KLispErrNotInteger);
    return str;

}

void* LispArgGetter::GetVoidStruct(LispInt aEvaluate, LispCharPtr aTypeString)
{
    GenericClass *gen;

    GoNext();
    if (aEvaluate)
    {
        LispPtr result;
        InternalEval(iEnvironment, result, *iIter.Ptr());
        gen = result.Get()->Generic();
    }
    else
    {
        gen = iIter()->Generic();
    }
    CheckArgType(gen != NULL ,iNrArgsParsed, iArguments,iEnvironment, KLispErrInvalidArg);
    CheckArgType(StrEqual(gen->TypeName(),aTypeString) ,iNrArgsParsed, iArguments,iEnvironment, KLispErrInvalidArg);
    GenericStruct *str = (GenericStruct*)gen;
    return str->Data();
}


LispInt LispArgGetter::GetShortIntegerArgument(LispInt aEvaluate)
{
    LispStringPtr str = GetIntegerArgument(aEvaluate);
    return InternalAsciiToInt(str->String());
}

LispStringPtr LispArgGetter::GetStringArgument(LispInt aEvaluate)
{
    GoNext();
    LispStringPtr str;
    if (aEvaluate)
    {
        LispPtr result;
        InternalEval(iEnvironment, result, *iIter.Ptr());
        str = result.Get()->String();
    }
    else
    {
        str = iIter()->String();
    }
    
    CheckArgType(InternalIsString(str) ,iNrArgsParsed, iArguments,
                 iEnvironment,KLispErrNotString);
    return iEnvironment.HashTable().LookUpUnStringify(str->String());
}



LispStringPtr LispArgGetter::GetAtomArgument(LispInt aEvaluate)
{
    GoNext();
    LispStringPtr str;
    if (aEvaluate)
    {
        LispPtr result;
        InternalEval(iEnvironment, result, *iIter.Ptr());
        str = result.Get()->String();
    }
    else
    {
        str = iIter()->String();
    }
    CheckArgType(str != NULL,iNrArgsParsed, iArguments,
                 iEnvironment,KLispErrInvalidArg);
    return iEnvironment.HashTable().LookUp(str->String());
}




void LispArgGetter::GetListArgument(LispPtr& aResult, LispInt aEvaluate)
{
    GoNext();
    if (aEvaluate)
    {
        InternalEval(iEnvironment, aResult, *iIter.Ptr());
    }
    else
    {
        aResult.Set(iIter());
    }
    CheckArgType(aResult.Get() != NULL ,iNrArgsParsed, iArguments,iEnvironment,
                 KLispErrNotList);
    CheckArgType(aResult.Get()->SubList() != NULL ,iNrArgsParsed, iArguments,iEnvironment,
                 KLispErrNotList);
}
void LispArgGetter::Finalize(LispInt aNrArgsNeeded)
{
    iIter.GoNext();
    if (iIter() != NULL)
    {
        iNrArgsNeeded = aNrArgsNeeded;
        iNrArgsParsed++;
        while (1) GoNext();
    }
}










void ReturnShortInteger(LispEnvironment& aEnvironment, LispPtr& aResult, LispInt r)
{
    char s[100];
    InternalIntToAscii(s,r);
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(s)));
}

void SetShortIntegerConstant(LispEnvironment& aEnvironment,
                                    LispCharPtr aName,
                                    LispInt aValue)
{
    LispPtr value;
    ReturnShortInteger(aEnvironment,value, aValue);
    aEnvironment.SetVariable( aEnvironment.HashTable().LookUp(aName),value);
}
double GetDoubleFloatArgument(LispArgGetter& g,LispInt aEvaluate)
{
    LispStringPtr str = g.GetAtomArgument(aEvaluate);
    return GetDouble(str->String());
}

void ReturnDoubleFloat(LispEnvironment& aEnvironment,
                              LispPtr& aResult, double r)
{
    aResult.Set(LispAtom::New(aEnvironment,Double(r, aEnvironment.HashTable())));
}

void ReturnVoidStruct(LispEnvironment& aEnvironment,
                      LispPtr& aResult,
                      LispCharPtr aName,
                      void* aData,
                      void (*aFree)(void*))
{
    aResult.Set(LispGenericClass::New(NEW GenericStruct(aName,aData,aFree)));
}

#endif


