
#ifndef __patternclass_h__
#define __patternclass_h__

#include "lisptype.h"
#include "lispobject.h"
#include "genericobject.h"
#include "patterns.h"

class PatternClass : public GenericClass
{
public:
    PatternClass(YacasPatternPredicateBase* aPatternMatcher);
    ~PatternClass();

    LispBoolean Matches(LispEnvironment& aEnvironment,
                        LispPtr& aArguments);
    LispBoolean Matches(LispEnvironment& aEnvironment,
                        LispPtr* aArguments);
public: //From GenericClass
    virtual LispCharPtr Send(LispArgList& aArgList);
    virtual LispCharPtr TypeName();
protected:
    YacasPatternPredicateBase* iPatternMatcher;
};




#endif

