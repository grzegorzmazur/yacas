
#include "yacasprivate.h"
#include "lispevalhash.h"

void LispEvaluator::Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
                             LispPtr& aArguments)
{
    iCaller(aEnvironment,aResult,aArguments);
}

