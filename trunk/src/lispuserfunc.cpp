
#include "yacas/yacasprivate.h"
#include "yacas/lispuserfunc.h"
#include "yacas/standard.h"

LispUserFunction::~LispUserFunction()
{
}

LispUserFunction* LispMultiUserFunction::UserFunc(LispInt aArity)
{
    LispInt i;
    //Find function body with the right arity
    LispInt nrc=iFunctions.Size();
    for (i=0;i<nrc;i++)
    {
        assert(iFunctions[i]);
        if (iFunctions[i]->IsArity(aArity))
        {
            return iFunctions[i];
        }
    }

    // if function not found, just unaccept!
    // User-defined function not found! Returning nullptr
    return nullptr;
}

void LispMultiUserFunction::DeleteBase(LispInt aArity)
{
    LispInt i;
    //Find function body with the right arity
    LispInt nrc=iFunctions.Size();
    for (i=0;i<nrc;i++)
    {
        assert(iFunctions[i]);
        if (iFunctions[i]->IsArity(aArity))
        {
            delete iFunctions[i];
            iFunctions[i] = nullptr;
            iFunctions.Delete(i,1);
            return;
        }
    }
}


LispMultiUserFunction::~LispMultiUserFunction()
{
}

void LispMultiUserFunction::HoldArgument(LispString * aVariable)
{
    LispInt i;
    for (i=0;i<iFunctions.Size();i++)
    {
        assert(iFunctions[i]);
        iFunctions[i]->HoldArgument(aVariable);
    }
}

void LispMultiUserFunction::DefineRuleBase(LispArityUserFunction* aNewFunction)
{
    LispInt i;
    //Find function body with the right arity
    LispInt nrc=iFunctions.Size();
    for (i=0;i<nrc;i++)
    {
        assert(iFunctions[i]);
        assert(aNewFunction);
        if (iFunctions[i]->IsArity(aNewFunction->Arity()) || aNewFunction->IsArity(iFunctions[i]->Arity()))
            throw LispErrArityAlreadyDefined();

    }
    iFunctions.Append(aNewFunction);
}

