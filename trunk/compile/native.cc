
#include "lispasem.h"
#include "lisperror.h"
#include "errors.h"
#include "arggetter.h"

void LispNativeFunctions::GetPrecision_0(LispEnvironment& aEnvironment)
{
    SET(TOP(0), MAKEINT(aEnvironment.Precision()));
}

void LispNativeFunctions::Precision_1(LispEnvironment& aEnvironment)
{
//TODO    TESTARGS(2);
//TODO    CHK_ARG(TOP(0).Get() != NULL, 1);
//TODO    CHK_ARG(TOP(0).Get()->String() != NULL, 1);
    LispInt ind = MakeGetInteger(TOP(0));
//TODO    CHK_ARG(ind>0,1);
    aEnvironment.SetPrecision(ind);
    POP();
    SETTRUE(TOP(0));
}

