
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
    LispInt ind = MakeGetInteger(TOP(0));
    aEnvironment.SetPrecision(ind);
    POP();
    SETTRUE(TOP(0));
}

