
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
#include "arrayclass.h"
#include "patternclass.h"
#include "substitute.h"
#include "errors.h"

#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)


void LispSubst(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr from (ARGUMENT(1));
    LispPtr to   (ARGUMENT(2));
    LispPtr body (ARGUMENT(3));
    SubstBehaviour behaviour(aEnvironment,from, to);
    InternalSubstitute(RESULT, body, behaviour);
}


void LispLocalSymbols(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInt nrArguments = InternalListLength(ARGUMENT(0));

    LispInt nrSymbols = nrArguments-2;

    LispString * *names      = PlatAllocN<LispString *>(nrSymbols);
    LispString * *localnames = PlatAllocN<LispString *>(nrSymbols);
    //TODO names and localnames should be pushed on the cleanup stack!!!
    CHK_CORE(names,KLispErrNotEnoughMemory);
    CHK_CORE(localnames,KLispErrNotEnoughMemory);

    LispInt uniquenumber = aEnvironment.GetUniqueId();
    LispInt i;
    for (i=0;i<nrSymbols;i++)
    {
        LispString * atomname = Argument(ARGUMENT(0), i+1)->String();
        CHK_ARG_CORE(atomname, i+1);
        names[i] = atomname;

        LispInt len = atomname->Size()-1;
        CHK_ARG_CORE(len<64,i+1);
        char newname[100];
        newname[0] = '$';
        PlatMemCopy(&newname[1], atomname->c_str(), len);

        InternalIntToAscii(&newname[1+len],uniquenumber);
        LispString * variable = aEnvironment.HashTable().LookUp(newname);
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
  LispString * str = ARGUMENT(1)->String();
  CHK_ARG_CORE(str,2);
  CHK_ARG_CORE(IsNumber(str->c_str(),LispFalse),2);
  LispInt asciiCode = InternalAsciiToInt(str);

  LispChar ascii[4];
  ascii[0] = '\"';
  ascii[1] = (LispChar)asciiCode;
  ascii[2] = '\"';
  ascii[3] = '\0';
  RESULT = (LispAtom::New(aEnvironment,ascii));
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
  LispChar * file = ARGUMENT(1)->iFileName;
  if (!file) file = "";
  LispString * str = aEnvironment.HashTable().LookUpStringify(file);
  RESULT = (LispAtom::New(aEnvironment,str->c_str()));
#endif
}

void LispDebugLine(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef YACAS_DEBUG
  RaiseError("Cannot call DebugLine in non-debug version of Yacas");
#else
  LispChar number[30];
  InternalIntToAscii(number,ARGUMENT(1)->iLine);
  RESULT = (LispAtom::New(aEnvironment,number));
#endif
}
