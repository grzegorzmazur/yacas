
#include "yacasprivate.h"
#include "lispuserfunc.h"
#include "standard.h"

LispUserFunction::~LispUserFunction()
{
}

LispUserFunction* LispMultiUserFunction::UserFunc(LispInt aArity)
{
    LispInt i;
    //Find function body with the right arity
    LispInt nrc=iFunctions.NrItems();
    for (i=0;i<nrc;i++)
    {
        LISPASSERT(iFunctions[i] != NULL);
        if (iFunctions[i]->IsArity(aArity))
        {
            return iFunctions[i];
        }
    }

    // if function not found, just unaccept!
    // User-defined function not found! Returning NULL
    return NULL;
}

void LispMultiUserFunction::DeleteBase(LispInt aArity)
{
    LispInt i;
    //Find function body with the right arity
    LispInt nrc=iFunctions.NrItems();
    for (i=0;i<nrc;i++)
    {
        LISPASSERT(iFunctions[i] != NULL);
        if (iFunctions[i]->IsArity(aArity))
        {
            delete iFunctions[i];
            iFunctions[i] = NULL;
            iFunctions.Delete(i,1);
            return;
        }
    }
}


LispMultiUserFunction::~LispMultiUserFunction()
{
}

void LispMultiUserFunction::HoldArgument(LispStringPtr aVariable)
{
    LispInt i;
    for (i=0;i<iFunctions.NrItems();i++)
    {
        LISPASSERT(iFunctions[i] != NULL);
        iFunctions[i]->HoldArgument(aVariable);
    }
}

void LispMultiUserFunction::DefineRuleBase(LispArityUserFunction* aNewFunction)
{
    LispInt i;
    //Find function body with the right arity
    LispInt nrc=iFunctions.NrItems();
    for (i=0;i<nrc;i++)
    {
        LISPASSERT(iFunctions[i] != NULL);
        LISPASSERT(aNewFunction != NULL);
        Check(!iFunctions[i]->IsArity(aNewFunction->Arity()),KLispErrArityAlreadyDefined);
        Check(!aNewFunction->IsArity(iFunctions[i]->Arity()),KLispErrArityAlreadyDefined);
    }
    iFunctions.Append(aNewFunction);
}

