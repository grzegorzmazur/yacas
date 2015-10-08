
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

const LispChar * PatternClass::TypeName() const
{
    return "\"Pattern\"";
}

bool PatternClass::Matches(LispEnvironment& aEnvironment,
                                  LispPtr& aArguments)
{
    assert(iPatternMatcher);
    return iPatternMatcher->Matches(aEnvironment, aArguments);
}

bool PatternClass::Matches(LispEnvironment& aEnvironment,
                                  LispPtr* aArguments)
{
    assert(iPatternMatcher);
    return iPatternMatcher->Matches(aEnvironment, aArguments);
}
