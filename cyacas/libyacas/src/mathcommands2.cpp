#include "yacas/arrayclass.h"
#include "yacas/errors.h"
#include "yacas/infixparser.h"
#include "yacas/lispatom.h"
#include "yacas/lispenvironment.h"
#include "yacas/lisperror.h"
#include "yacas/lispeval.h"
#include "yacas/lispparser.h"
#include "yacas/lispuserfunc.h"
#include "yacas/mathuserfunc.h"
#include "yacas/numbers.h"
#include "yacas/patternclass.h"
#include "yacas/platmath.h"
#include "yacas/standard.h"
#include "yacas/substitute.h"

#include <cstring>

#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack[aStackTop]
#define ARGUMENT(i) aEnvironment.iStack[aStackTop + i]

void LispSubst(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr from(ARGUMENT(1));
    LispPtr to(ARGUMENT(2));
    LispPtr body(ARGUMENT(3));
    SubstBehaviour behaviour(aEnvironment, from, to);
    InternalSubstitute(RESULT, body, behaviour);
}

void LispLocalSymbols(LispEnvironment& aEnvironment, int aStackTop)
{
    int nrArguments = InternalListLength(ARGUMENT(0));

    int nrSymbols = nrArguments - 2;

    std::vector<const LispString*> names(nrSymbols);
    std::vector<const LispString*> localnames(nrSymbols);

    int uniquenumber = aEnvironment.GetUniqueId();
    int i;
    for (i = 0; i < nrSymbols; i++) {
        const LispString* atomname = Argument(ARGUMENT(0), i + 1)->String();
        CheckArg(atomname, i + 1, aEnvironment, aStackTop);
        names[i] = atomname;

        std::string newname = "$";
        newname.append(*atomname);
        newname.append(std::to_string(uniquenumber));
        localnames[i] = aEnvironment.HashTable().LookUp(newname);
    }

    LocalSymbolBehaviour behaviour(
        aEnvironment, std::move(names), std::move(localnames));
    LispPtr result;
    InternalSubstitute(
        result, Argument(ARGUMENT(0), nrArguments - 1), behaviour);

    InternalEval(aEnvironment, RESULT, result);
}

void LispCharString(LispEnvironment& aEnvironment, int aStackTop)
{
    const LispString* str = ARGUMENT(1)->String();
    CheckArg(str, 2, aEnvironment, aStackTop);
    CheckArg(IsNumber(*str, false), 2, aEnvironment, aStackTop);
    int asciiCode = InternalAsciiToInt(*str);

    char ascii[4];
    ascii[0] = '\"';
    ascii[1] = (char)asciiCode;
    ascii[2] = '\"';
    ascii[3] = '\0';
    RESULT = (LispAtom::New(aEnvironment, ascii));
}

void LispInDebugMode(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalFalse(aEnvironment, RESULT);
}

void LispDebugFile(LispEnvironment& aEnvironment, int aStackTop)
{
    throw LispErrGeneric("Cannot call DebugFile in non-debug version of Yacas");
}

void LispDebugLine(LispEnvironment& aEnvironment, int aStackTop)
{
    throw LispErrGeneric("Cannot call DebugLine in non-debug version of Yacas");
}
