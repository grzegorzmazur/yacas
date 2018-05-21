#ifndef YACAS_ERRORS_H
#define YACAS_ERRORS_H

#include "lispenvironment.h"

void CheckArg(bool pred, int arg_idx, LispEnvironment& env, int stack_top);

void CheckArgIsString(LispPtr arg, int arg_idx, LispEnvironment& env, int stack_top);
void CheckArgIsString(int arg_idx, LispEnvironment& env, int stack_top);

void CheckArgIsList(LispPtr arg, int arg_idx, LispEnvironment& env, int stack_top);
void CheckArgIsList(int arg_idx, LispEnvironment& env, int stack_top);

void CheckNrArgs(int n, LispPtr& aArguments, LispEnvironment& aEnvironment);

void ShowStack(LispEnvironment& aEnvironment);

void ShowFunctionError(LispPtr& aArguments, LispEnvironment& aEnvironment);


void CheckSecure(LispEnvironment& env, int stack_top);


#endif
