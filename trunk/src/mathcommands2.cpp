
#include "yacasprivate.h"

#include "yacasbase.h"
#include "lispenvironment.h"
#include "standard.h"
#include "lispeval.h"
#include "lispatom.h"
#include "lispparser.h"
#include "stdfileio.h"
#include "stringio.h"
#include "lisperror.h"
#include "infixparser.h"
#include "lispuserfunc.h"
#include "mathuserfunc.h"
#include "platmath.h"
#include "numbers.h"
#include "anumber.h"
#include "arrayclass.h"
#include "patternclass.h"
#include "substitute.h"
#include "errors.h"

#define InternalEval aEnvironment.iEvaluator->Eval


void LispSubst(LispEnvironment& aEnvironment, LispPtr& aResult,
               LispPtr& aArguments)
{
    TESTARGS(4);

    LispPtr from,to,body;
    InternalEval(aEnvironment, from, Argument(aArguments,1));
    InternalEval(aEnvironment, to  , Argument(aArguments,2));
    InternalEval(aEnvironment, body, Argument(aArguments,3));
    SubstBehaviour behaviour(aEnvironment,from, to);
    InternalSubstitute(aResult, body, behaviour);
}


void LispLocalSymbols(LispEnvironment& aEnvironment, LispPtr& aResult,
                      LispPtr& aArguments)
{
    LispInt nrArguments = InternalListLength(aArguments);

    LispInt nrSymbols = nrArguments-2;

    LispStringPtr *names = (LispStringPtr *)PlatAlloc(sizeof(LispStringPtr)*nrSymbols);
    LispStringPtr *localnames = (LispStringPtr *)PlatAlloc(sizeof(LispStringPtr)*nrSymbols);
    //TODO names and localnames should be pushed on the cleanup stack!!!
    CHK(names != NULL,KLispErrNotEnoughMemory);
    CHK(localnames != NULL,KLispErrNotEnoughMemory);

    LispInt uniquenumber = aEnvironment.GetUniqueId();
    LispInt i;
    for (i=0;i<nrSymbols;i++)
    {
        LispStringPtr atomname = Argument(aArguments, i+1).Get()->String();
        CHK_ARG(atomname != NULL, i+1);
        names[i] = atomname;

        LispInt len = atomname->NrItems()-1;
        CHK_ARG(len<64,i+1);
        char newname[100];
        newname[0] = '$';
        PlatMemCopy(&newname[1], atomname->String(), len);

        InternalIntToAscii(&newname[1+len],uniquenumber);
        LispStringPtr variable = aEnvironment.HashTable().LookUp(newname);
        localnames[i] = variable;
    }
    

    LocalSymbolBehaviour behaviour(aEnvironment,names,localnames,nrSymbols);
    LispPtr result;
    InternalSubstitute(result, Argument(aArguments, nrArguments-1), behaviour);
    PlatFree(names);
    PlatFree(localnames);


    InternalEval(aEnvironment, aResult, result);
}

