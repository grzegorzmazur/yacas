
#include "yacas/yacasprivate.h"

#include "yacas/yacasbase.h"
#include "yacas/lispenvironment.h"
#include "yacas/standard.h"
#include "yacas/lispeval.h"
#include "yacas/lispatom.h"
#include "yacas/lispparser.h"
#include "yacas/stdfileio.h"
#include "yacas/stringio.h"
#include "yacas/lisperror.h"
#include "yacas/infixparser.h"
#include "yacas/lispuserfunc.h"
#include "yacas/mathuserfunc.h"
#include "yacas/platmath.h"
#include "yacas/numbers.h"
#include "yacas/arrayclass.h"
#include "yacas/patternclass.h"
#include "yacas/substitute.h"
#include "yacas/errors.h"

#include <cstring>

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

    std::vector<const LispString*> names(nrSymbols);
    std::vector<const LispString*> localnames(nrSymbols);

    LispInt uniquenumber = aEnvironment.GetUniqueId();
    LispInt i;
    for (i=0;i<nrSymbols;i++)
    {
        const LispString* atomname = Argument(ARGUMENT(0), i+1)->String();
        CheckArg(atomname, i + 1, aEnvironment, aStackTop);
        names[i] = atomname;

        std::string newname = "$";
        newname.append(*atomname);
        newname.append(std::to_string(uniquenumber));
        localnames[i] = aEnvironment.HashTable().LookUp(newname);
    }

    LocalSymbolBehaviour behaviour(aEnvironment, std::move(names), std::move(localnames));
    LispPtr result;
    InternalSubstitute(result, Argument(ARGUMENT(0), nrArguments-1), behaviour);

    InternalEval(aEnvironment, RESULT, result);
}



void LispCharString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  const LispString* str = ARGUMENT(1)->String();
  CheckArg(str, 2, aEnvironment, aStackTop);
  CheckArg(IsNumber(str->c_str(), false), 2, aEnvironment, aStackTop);
  LispInt asciiCode = InternalAsciiToInt(*str);

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
    throw LispErrGeneric("Cannot call DebugFile in non-debug version of Yacas");
#else
  const LispChar * file = ARGUMENT(1)->iFileName;
  if (!file) file = "";
  LispString * str = aEnvironment.HashTable().LookUpStringify(file);
  RESULT = LispAtom::New(aEnvironment, *str);
#endif
}

void LispDebugLine(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef YACAS_DEBUG
    throw LispErrGeneric("Cannot call DebugLine in non-debug version of Yacas");
#else
  RESULT = LispAtom::New(aEnvironment, std::to_string(ARGUMENT(1)->iLine));
#endif
}
