
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
//TODO remove #include "anumber.h"
#include "arrayclass.h"
#include "patternclass.h"
#include "substitute.h"
#include "errors.h"

#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)


void LispSubst(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(4);

    LispPtr from,to,body;
    from.Set(ARGUMENT(1).Get());
    to  .Set(ARGUMENT(2).Get());
    body.Set(ARGUMENT(3).Get());
    SubstBehaviour behaviour(aEnvironment,from, to);
    InternalSubstitute(RESULT, body, behaviour);
}


void LispLocalSymbols(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInt nrArguments = InternalListLength(ARGUMENT(0));

    LispInt nrSymbols = nrArguments-2;

    LispStringPtr *names = (LispStringPtr *)PlatAlloc(sizeof(LispStringPtr)*nrSymbols);
    LispStringPtr *localnames = (LispStringPtr *)PlatAlloc(sizeof(LispStringPtr)*nrSymbols);
    //TODO names and localnames should be pushed on the cleanup stack!!!
    CHK_CORE(names != NULL,KLispErrNotEnoughMemory);
    CHK_CORE(localnames != NULL,KLispErrNotEnoughMemory);

    LispInt uniquenumber = aEnvironment.GetUniqueId();
    LispInt i;
    for (i=0;i<nrSymbols;i++)
    {
        LispStringPtr atomname = Argument(ARGUMENT(0), i+1).Get()->String();
        CHK_ARG_CORE(atomname != NULL, i+1);
        names[i] = atomname;

        LispInt len = atomname->NrItems()-1;
        CHK_ARG_CORE(len<64,i+1);
        char newname[100];
        newname[0] = '$';
        PlatMemCopy(&newname[1], atomname->String(), len);

        InternalIntToAscii(&newname[1+len],uniquenumber);
        LispStringPtr variable = aEnvironment.HashTable().LookUp(newname);
        localnames[i] = variable;
    }
    

    LocalSymbolBehaviour behaviour(aEnvironment,names,localnames,nrSymbols);
    LispPtr result;
    InternalSubstitute(result, Argument(ARGUMENT(0), nrArguments-1), behaviour);
    PlatFree(names);
    PlatFree(localnames);


    InternalEval(aEnvironment, RESULT, result);
}



void LispCharString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispStringPtr str;
  str = ARGUMENT(1).Get()->String();
  CHK_ARG_CORE(str != NULL,2);
  CHK_ARG_CORE(IsNumber(str->String(),LispFalse),2);
  LispInt asciiCode = InternalAsciiToInt(str->String());

  LispChar ascii[4];
  ascii[0] = '\"';
  ascii[1] = (LispChar)asciiCode;
  ascii[2] = '\"';
  ascii[3] = '\0';
  RESULT.Set(LispAtom::New(aEnvironment,ascii));
}


void LispInDebugMode(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifdef YACAS_DEBUG
  InternalTrue(aEnvironment,RESULT);
#else
  InternalFalse(aEnvironment,RESULT);
#endif
}

void LispDebugFile(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef YACAS_DEBUG
  RaiseError("Cannot call DebugFile in non-debug version of Yacas");
#else
  LispCharPtr file = ARGUMENT(1).Get()->iFileName;
  if (file == NULL) file = "";
  LispStringPtr str = aEnvironment.HashTable().LookUpStringify(file);
  RESULT.Set(LispAtom::New(aEnvironment,str->String()));
#endif
}

void LispDebugLine(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef YACAS_DEBUG
  RaiseError("Cannot call DebugLine in non-debug version of Yacas");
#else
  LispChar number[30];
  InternalIntToAscii(number,ARGUMENT(1).Get()->iLine);
  RESULT.Set(LispAtom::New(aEnvironment,number));
#endif
}

