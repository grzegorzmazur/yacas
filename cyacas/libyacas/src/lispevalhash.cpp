

#include "yacas/lispevalhash.h"
#include "yacas/errors.h"
#include "yacas/lispatom.h"
#include "yacas/lispenvironment.h"
#include "yacas/lispeval.h"

void YacasEvaluator::Evaluate(LispPtr& aResult,
                              LispEnvironment& aEnvironment,
                              LispPtr& aArguments) const
{

    if (!(iFlags & Variable)) {
        CheckNrArgs(iNrArgs + 1, aArguments, aEnvironment);
    }

    int stacktop = aEnvironment.iStack.size();

    // Push a place holder for the result: push full expression so it is
    // available for error reporting
    aEnvironment.iStack.push_back(aArguments);

    LispIterator iter(aArguments);
    ++iter;

    int i;
    int nr = iNrArgs;

    if (iFlags & Variable)
        nr--;

    // Walk over all arguments, evaluating them as necessary
    if (iFlags & Macro) {
        for (i = 0; i < nr; i++) {
            if (!iter.getObj())
                throw LispErrWrongNumberOfArgs();

            aEnvironment.iStack.push_back(LispPtr(iter.getObj()->Copy()));
            ++iter;
        }
        if (iFlags & Variable) {
            LispPtr head(aEnvironment.iList->Copy());
            head->Nixed() = (iter.getObj());
            aEnvironment.iStack.push_back(LispPtr(LispSubList::New(head)));
        }
    } else {
        LispPtr arg;
        for (i = 0; i < nr; i++) {
            if (!iter.getObj())
                throw LispErrWrongNumberOfArgs();

            aEnvironment.iEvaluator->Eval(aEnvironment, arg, *iter);
            aEnvironment.iStack.push_back(arg);
            ++iter;
        }
        if (iFlags & Variable) {
            LispPtr head(aEnvironment.iList->Copy());
            head->Nixed() = (iter.getObj());
            LispPtr list(LispSubList::New(head));

            aEnvironment.iEvaluator->Eval(aEnvironment, arg, list);
            aEnvironment.iStack.push_back(arg);
        }
    }

    iCaller(aEnvironment, stacktop);
    aResult = (aEnvironment.iStack[stacktop]);
    aEnvironment.iStack.resize(stacktop);
}
