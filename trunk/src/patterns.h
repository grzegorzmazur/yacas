
#ifndef __patterns_h__
#define __patterns_h__

// pattern matching code.
//
// General idea: have a class that can match function parameters
// to a pattern, check for predicates on the arguments, and return
// whether there was a match.
//
// First the pattern is mapped onto the arguments. Then local variables
// are set. Then the predicates are called. If they all return true,
// Then the pattern matches, and the locals can stay (the body is expected
// to use these variables).
//

#include "yacasbase.h"
#include "lisptype.h"
#include "grower.h"
#include "lispenvironment.h"

class YacasParamMatcherBase : public YacasBase
{
public:
    virtual ~YacasParamMatcherBase();
    virtual LispBoolean ArgumentMatches(LispEnvironment& aEnvironment,
                                        LispPtr& aExpression,
                                        LispPtr* arguments)=0;
};

class MatchAtom : public YacasParamMatcherBase
{
public:
    MatchAtom(LispStringPtr aString);
    virtual LispBoolean ArgumentMatches(LispEnvironment& aEnvironment,
                                        LispPtr& aExpression,
                                        LispPtr* arguments);
protected:
    LispStringPtr iString;
};

class MatchSubList : public YacasParamMatcherBase
{
public:
    MatchSubList(YacasParamMatcherBase** aMatchers, LispInt aNrMatchers);
    ~MatchSubList();
    virtual LispBoolean ArgumentMatches(LispEnvironment& aEnvironment,
                                        LispPtr& aExpression,
                                        LispPtr* arguments);
protected:
    YacasParamMatcherBase** iMatchers;
    LispInt iNrMatchers;
};


class MatchVariable : public YacasParamMatcherBase
{
public:
    MatchVariable(LispInt aVarIndex);
    virtual LispBoolean ArgumentMatches(LispEnvironment& aEnvironment,
                                        LispPtr& aExpression,
                                        LispPtr* arguments);
protected:
    LispInt iVarIndex;
    LispStringPtr iString;
};


// YacasPatternPredicateBase defines the interface to an object
// that matches a set of arguments to a function to a pattern.
//
class YacasPatternPredicateBase : public YacasBase
{
public:
    YacasPatternPredicateBase(LispEnvironment& aEnvironment,
                              LispPtr& aPattern,
                              LispPtr& aPostPredicate);
    ~YacasPatternPredicateBase();
    LispBoolean Matches(LispEnvironment& aEnvironment,
                        LispPtr& aArguments);
    LispBoolean Matches(LispEnvironment& aEnvironment,
                        LispPtr* aArguments);

protected:
    YacasParamMatcherBase* MakeParamMatcher(LispEnvironment& aEnvironment, LispObject* aPattern);
    LispInt LookUp(LispStringPtr aVariable);

protected:
    void SetPatternVariables(LispEnvironment& aEnvironment, LispPtr* arguments);
    LispBoolean CheckPredicates(LispEnvironment& aEnvironment);

protected:
    CDeletingArrayGrower<YacasParamMatcherBase*> iParamMatchers;
    CArrayGrower<LispStringPtr> iVariables;
    CDeletingArrayGrower<LispPtr*> iPredicates;
};


#endif
