
#include "patternclass.h"

PatternClass::PatternClass(YacasPatternPredicateBase* aPatternMatcher)
: iPatternMatcher(aPatternMatcher)
{
}

PatternClass::~PatternClass()
{
    delete iPatternMatcher;
}

LispCharPtr PatternClass::Send(LispArgList& aArgList)
{
    return NULL;
}

LispCharPtr PatternClass::TypeName()
{
    return "\"Pattern\"";
}

LispBoolean PatternClass::Matches(LispEnvironment& aEnvironment,
                                  LispPtr& aArguments)
{
    LISPASSERT(iPatternMatcher != NULL);
    LispBoolean result;
    result = iPatternMatcher->Matches(aEnvironment, aArguments);
    return result;
}

LispBoolean PatternClass::Matches(LispEnvironment& aEnvironment,
                                  LispPtr* aArguments)
{
    LISPASSERT(iPatternMatcher != NULL);
    LispBoolean result;
    result = iPatternMatcher->Matches(aEnvironment, aArguments);
    return result;
}



