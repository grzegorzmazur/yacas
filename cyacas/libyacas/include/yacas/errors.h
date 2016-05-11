#ifndef YACAS_ERRORS_H
#define YACAS_ERRORS_H

void CheckArg(bool pred, LispInt arg_idx, LispEnvironment& env, LispInt stack_top);

void CheckArgIsString(LispPtr arg, LispInt arg_idx, LispEnvironment& env, LispInt stack_top);
void CheckArgIsString(LispInt arg_idx, LispEnvironment& env, LispInt stack_top);

void CheckArgIsList(LispPtr arg, LispInt arg_idx, LispEnvironment& env, LispInt stack_top);
void CheckArgIsList(LispInt arg_idx, LispEnvironment& env, LispInt stack_top);

void CheckNrArgs(LispInt n, LispPtr& aArguments, LispEnvironment& aEnvironment);

void ShowStack(LispEnvironment& aEnvironment);

void ShowFunctionError(LispPtr& aArguments, LispEnvironment& aEnvironment);


void CheckSecure(LispEnvironment& env, LispInt stack_top);


#endif
