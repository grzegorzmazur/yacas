#include "yacas/errors.h"
#include "yacas/infixparser.h"
#include "yacas/lispenvironment.h"
#include "yacas/lispeval.h"
#include "yacas/standard.h"

void ShowStack(LispEnvironment& aEnvironment)
{
    aEnvironment.iEvaluator->ShowStack(aEnvironment, aEnvironment.iErrorOutput);
}

void ShowFunctionError(LispPtr& aArguments, LispEnvironment& aEnvironment)
{
    if (const LispString* string = aArguments->String())
        aEnvironment.iErrorOutput << "In function \"" << *string << "\" : \n";
}

void ShowArgTypeErrorInfo(int aArgNr,
                          LispPtr& aArguments,
                          LispEnvironment& aEnvironment)
{
    ShowStack(aEnvironment);
    ShowFunctionError(aArguments, aEnvironment);

    aEnvironment.iErrorOutput << "bad argument number " << aArgNr
                              << " (counting from 1)\n";

    const int LIM_AL = 60;

    LispPtr& arg = Argument(aArguments, aArgNr);
    LispString strout;

    PrintExpression(strout, arg, aEnvironment, LIM_AL);
    aEnvironment.iErrorOutput << "The offending argument " << strout;

    LispPtr eval;
    aEnvironment.iEvaluator->Eval(aEnvironment, eval, arg);
    PrintExpression(strout, eval, aEnvironment, LIM_AL);

    aEnvironment.iErrorOutput << " evaluated to " << strout << '\n';
}

void CheckArg(bool pred, int arg_idx, LispEnvironment& env, int stack_top)
{
    if (!pred) {
        ShowArgTypeErrorInfo(arg_idx, env.iStack[stack_top], env);
        throw LispErrInvalidArg();
    }
}

void CheckArgIsString(LispPtr arg,
                      int arg_idx,
                      LispEnvironment& env,
                      int stack_top)
{
    if (!InternalIsString(arg->String())) {
        ShowArgTypeErrorInfo(arg_idx, env.iStack[stack_top], env);
        throw LispErrNotString();
    }
}

void CheckArgIsString(int arg_idx, LispEnvironment& env, int stack_top)
{
    CheckArgIsString(env.iStack[stack_top + arg_idx], arg_idx, env, stack_top);
}

void CheckArgIsList(LispPtr arg,
                    int arg_idx,
                    LispEnvironment& env,
                    int stack_top)
{
    if (!InternalIsList(env, arg)) {
        ShowArgTypeErrorInfo(arg_idx, env.iStack[stack_top], env);
        throw LispErrNotList();
    }
}

void CheckArgIsList(int arg_idx, LispEnvironment& env, int stack_top)
{
    CheckArgIsList(env.iStack[stack_top + arg_idx], arg_idx, env, stack_top);
}

void CheckNrArgs(int n, LispPtr& aArguments, LispEnvironment& aEnvironment)
{
    const int nrArguments = InternalListLength(aArguments);

    if (nrArguments == n)
        return;

    const int needed = n - 1;
    const int passed = nrArguments - 1;

    ShowStack(aEnvironment);
    ShowFunctionError(aArguments, aEnvironment);

    aEnvironment.iErrorOutput << "expected " << needed << " arguments, got "
                              << passed << "\n";

    throw LispErrWrongNumberOfArgs();
}

void CheckSecure(LispEnvironment& env, int stack_top)
{
    if (env.secure) {
        ShowStack(env);
        ShowFunctionError(env.iStack[stack_top], env);
        throw LispErrSecurityBreach();
    }
}
