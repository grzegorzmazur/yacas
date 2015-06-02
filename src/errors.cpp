
#include "yacas/yacasprivate.h"
#include "yacas/lispenvironment.h"
#include "yacas/standard.h"
#include "yacas/lispeval.h"
#include "yacas/errors.h"
#include "yacas/infixparser.h"

#include <stdio.h>

void ShowStack(LispEnvironment& aEnvironment)
{
    aEnvironment.iEvaluator->ShowStack(aEnvironment,
                                       aEnvironment.iErrorOutput);
}

void ShowFunctionError(LispPtr& aArguments,
                       LispEnvironment& aEnvironment)
{
    if (const LispString * string = aArguments->String())
        aEnvironment.iErrorOutput << "In function \"" << *string << "\" : \n";
}

void ShowArgTypeErrorInfo(LispInt aArgNr, LispPtr& aArguments, LispEnvironment& aEnvironment)
{
    ShowStack(aEnvironment);
    ShowFunctionError(aArguments, aEnvironment);

    aEnvironment.iErrorOutput << "bad argument number " << aArgNr << " (counting from 1)\n";

    const int LIM_AL = 60;

    LispPtr& arg = Argument(aArguments,aArgNr);
    LispString strout;

    PrintExpression(strout, arg, aEnvironment, LIM_AL);
    aEnvironment.iErrorOutput << "The offending argument " << strout;

    LispPtr eval;
    aEnvironment.iEvaluator->Eval(aEnvironment, eval, arg);
    PrintExpression(strout, eval, aEnvironment, LIM_AL);

    aEnvironment.iErrorOutput << " evaluated to " << strout << '\n';

    DBG_( printf("Problem occurred at %s(%d)\n",
                 aArguments->iFileName,
                 aArguments->iLine ); )
}

void CheckArg(bool pred, LispInt arg_idx, LispEnvironment& env, LispInt stack_top)
{
    if (!pred) {
        ShowArgTypeErrorInfo(arg_idx, env.iStack.GetElement(stack_top), env);
        throw LispErrInvalidArg();
    }
}

void CheckArgIsString(LispPtr arg, LispInt arg_idx, LispEnvironment& env, LispInt stack_top)
{
    if (!InternalIsString(arg->String())) {
        ShowArgTypeErrorInfo(arg_idx, env.iStack.GetElement(stack_top), env);
        throw LispErrNotString();
    }
}


void CheckArgIsString(LispInt arg_idx, LispEnvironment& env, LispInt stack_top)
{
    CheckArgIsString(env.iStack.GetElement(stack_top + arg_idx), arg_idx, env, stack_top);
}

void CheckArgIsList(LispPtr arg, LispInt arg_idx, LispEnvironment& env, LispInt stack_top)
{
    if (!InternalIsList(env, arg)) {
        ShowArgTypeErrorInfo(arg_idx, env.iStack.GetElement(stack_top), env);
        throw LispErrNotList();
    }
}


void CheckArgIsList(LispInt arg_idx, LispEnvironment& env, LispInt stack_top)
{
    CheckArgIsList(env.iStack.GetElement(stack_top + arg_idx), arg_idx, env, stack_top);
}

void CheckNrArgs(LispInt n, LispPtr& aArguments,
                 LispEnvironment& aEnvironment)
{
    const LispInt nrArguments = InternalListLength(aArguments);

    if (nrArguments == n)
        return;

    const LispInt needed = n-1;
    const LispInt passed = nrArguments-1;

    ShowStack(aEnvironment);
    ShowFunctionError(aArguments, aEnvironment);

    aEnvironment.iErrorOutput << "expected " << needed << " arguments, got " << passed << "\n";

    throw LispErrWrongNumberOfArgs();
}

void CheckSecure(LispEnvironment& env, LispInt stack_top)
{
    if (env.secure) {
        ShowStack(env);
        ShowFunctionError(env.iStack.GetElement(stack_top), env);
        throw LispErrSecurityBreach();
    }
}
