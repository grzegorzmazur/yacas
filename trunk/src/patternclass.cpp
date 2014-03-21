
#include "yacas/yacasprivate.h"
#include "yacas/patternclass.h"

PatternClass::PatternClass(YacasPatternPredicateBase* aPatternMatcher)
: iPatternMatcher(aPatternMatcher)
{
}

PatternClass::~PatternClass()
{
    delete iPatternMatcher;
}

const LispChar * PatternClass::Send(LispArgList& aArgList)
{
    return NULL;
}

const LispChar * PatternClass::TypeName()
{
    return "\"Pattern\"";
}

bool PatternClass::Matches(LispEnvironment& aEnvironment,
                                  LispPtr& aArguments)
{
    assert(iPatternMatcher);
    bool result;
    result = iPatternMatcher->Matches(aEnvironment, aArguments);
    return result;
}

bool PatternClass::Matches(LispEnvironment& aEnvironment,
                                  LispPtr* aArguments)
{
    assert(iPatternMatcher);
    bool result;
    result = iPatternMatcher->Matches(aEnvironment, aArguments);
    return result;
}



