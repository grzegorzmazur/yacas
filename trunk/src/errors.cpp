
#include "yacas/yacasprivate.h"
#include "yacas/lispenvironment.h"
#include "yacas/standard.h"
#include "yacas/lispeval.h"
#include "yacas/errors.h"
#include "yacas/infixparser.h"

#include <stdio.h>

#define InternalEval aEnvironment.iEvaluator->Eval

void ShowStack(LispEnvironment& aEnvironment)
{
    aEnvironment.iEvaluator->ShowStack(aEnvironment,
                                       aEnvironment.iErrorOutput);
}



void ShowFunctionError(LispPtr& aArguments,
                       LispEnvironment& aEnvironment)
{
  if (!aArguments)
  {
    aEnvironment.iErrorOutput.Write("Error in compiled code\n");
  }
  else
  {
    LispString * string = aArguments->String();
    if (string)
    {
      aEnvironment.iErrorOutput.Write("In function \"");
      aEnvironment.iErrorOutput.Write(string->c_str());
      aEnvironment.iErrorOutput.Write("\" : \n");
    }
  }
}

void ShowArgTypeErrorInfo(LispInt aArgNr, LispPtr& aArguments, LispEnvironment& aEnvironment)
{
    if (!aArguments) {
        aEnvironment.iErrorOutput.Write("Error in compiled code\n");
        return;
    }

    ShowStack(aEnvironment);
    ShowFunctionError(aArguments, aEnvironment);
 
    aEnvironment.iErrorOutput.Write("bad argument number ");
    LispChar str[20];
    InternalIntToAscii(str,aArgNr);
    aEnvironment.iErrorOutput.Write(str);
    aEnvironment.iErrorOutput.Write(" (counting from 1)\n");
 
    const int LIM_AL = 60;

    LispPtr& arg = Argument(aArguments,aArgNr);
    LispString strout;
 
    aEnvironment.iErrorOutput.Write("The offending argument ");
    PrintExpression(strout, arg, aEnvironment, LIM_AL);
    aEnvironment.iErrorOutput.Write(strout.c_str());
 
    LispPtr eval;
    aEnvironment.iEvaluator->Eval(aEnvironment, eval, arg);
    aEnvironment.iErrorOutput.Write(" evaluated to ");
    PrintExpression(strout, eval, aEnvironment, LIM_AL);
    aEnvironment.iErrorOutput.Write(strout.c_str());
    aEnvironment.iErrorOutput.Write("\n");

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
    if (!InternalIsList(arg)) {
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

    if (!aArguments) {
        aEnvironment.iErrorOutput.Write("Error in compiled code\n");
    } else {
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
    }

    throw LispErrWrongNumberOfArgs();
}

void CheckSecure(LispEnvironment& env, LispInt stack_top)
{
    if (env.iSecure) {
        ShowStack(env);
        ShowFunctionError(env.iStack.GetElement(stack_top), env);
        throw LispErrSecurityBreach();
    }
}

