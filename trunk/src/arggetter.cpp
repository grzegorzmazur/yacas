
#include "yacasprivate.h"
#include "lispenvironment.h"
#include "lispstring.h"
#include "standard.h"
#include "errors.h"
#include "lispeval.h"
#include "arggetter.h"
#include "platmath.h"
#include "genericstructs.h"

#define InternalEval iEnvironment.iEvaluator->Eval

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
    aResult.Set(LispAtom::New(aEnvironment.HashTable().LookUp(s)));
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
    aResult.Set(LispAtom::New(Double(r, aEnvironment.HashTable())));
}

void ReturnVoidStruct(LispEnvironment& aEnvironment,
                      LispPtr& aResult,
                      LispCharPtr aName,
                      void* aData,
                      void (*aFree)(void*))
{
    aResult.Set(LispGenericClass::New(new GenericStruct(aName,aData,aFree)));
}



