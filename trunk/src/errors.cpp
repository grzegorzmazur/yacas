
#include "yacasprivate.h"
#include "lispenvironment.h"
#include "standard.h"
#include "lispeval.h"
#include "errors.h"

void ShowStack(LispEnvironment& aEnvironment)
{
    aEnvironment.iEvaluator->ShowStack(aEnvironment,
                                       aEnvironment.iErrorOutput);
}



void ShowFunctionError(LispPtr& aArguments,
                       LispEnvironment& aEnvironment)
{
    LispStringPtr string = aArguments.Get()->String();
    if (string)
    {
        aEnvironment.iErrorOutput.Write("In function \"");
        aEnvironment.iErrorOutput.Write(string->String());
        aEnvironment.iErrorOutput.Write("\" : \n");
    }
}

void CheckNrArgs(LispInt n, LispPtr& aArguments,
                 LispEnvironment& aEnvironment)
{
    LispInt nrArguments = InternalListLength(aArguments);
    if (nrArguments != n)
    {
        ErrorNrArgs(n-1, nrArguments-1, aArguments, aEnvironment);
    }
}

void ErrorNrArgs(LispInt needed, LispInt passed, LispPtr& aArguments,
                 LispEnvironment& aEnvironment)
{
    ShowStack(aEnvironment);
    ShowFunctionError(aArguments, aEnvironment);

    LispChar str[20];
    aEnvironment.iErrorOutput.Write("expected ");
    InternalIntToAscii(str,needed);
    aEnvironment.iErrorOutput.Write(str);
    aEnvironment.iErrorOutput.Write(" arguments, got ");
    InternalIntToAscii(str,passed);
    aEnvironment.iErrorOutput.Write(str);
    aEnvironment.iErrorOutput.Write("\n");
    Check(passed == needed,KLispErrWrongNumberOfArgs);
}


void CheckFuncGeneric(LispInt aPredicate,LispInt aError,LispPtr& aArguments,
                      LispEnvironment& aEnvironment)
{
    if (!aPredicate)
    {
        ShowStack(aEnvironment);
        ShowFunctionError(aArguments, aEnvironment);
        Check(aPredicate,aError);
    }
}
void CheckFuncGeneric(LispInt aPredicate,LispInt aError,
                      LispEnvironment& aEnvironment)
{
    if (!aPredicate)
    {
        ShowStack(aEnvironment);
        Check(aPredicate,aError);
    }
}

/*TODO remove??? */
void CheckArgType(LispInt aPredicate, LispInt aArgNr, LispPtr& aArguments,
                  LispEnvironment& aEnvironment)
{
    CheckArgType(aPredicate, aArgNr, aArguments,
                 aEnvironment, KLispErrInvalidArg);
}
/* */

void CheckArgType(LispInt aPredicate, LispInt aArgNr, LispPtr& aArguments,LispEnvironment& aEnvironment,
                  LispInt aError)
{
    if (!aPredicate)
    {
        ShowStack(aEnvironment);
        ShowFunctionError(aArguments, aEnvironment);

        aEnvironment.iErrorOutput.Write("bad argument number ");
        LispChar str[20];
        InternalIntToAscii(str,aArgNr);
        aEnvironment.iErrorOutput.Write(str);
        aEnvironment.iErrorOutput.Write(" (counting from 1)\n");
        Check(aPredicate,aError);
    }
}






