
#ifndef __substitute_h__
#define __substitute_h__

#include "yacasbase.h"
#include "lispobject.h"


/** Behaviour for substituting sub-expressions.
 */
class SubstBehaviourBase : public YacasBase
{
public:
    virtual LispBoolean Matches(LispPtr& aResult, LispPtr& aElement) = 0;
};

/** main routine that can perform substituting of expressions
 */
void InternalSubstitute(LispPtr& aTarget, LispPtr& aSource,
                        SubstBehaviourBase& aBehaviour);


/** Substing one expression for another. The simplest form
 * of substitution
 */
class SubstBehaviour : public SubstBehaviourBase
{
public:
    SubstBehaviour(LispEnvironment& aEnvironment,LispPtr& aToMatch,
                  LispPtr& aToReplaceWith);
    virtual LispBoolean Matches(LispPtr& aResult, LispPtr& aElement);
private:
    LispEnvironment& iEnvironment;
    LispPtr& iToMatch;
    LispPtr& iToReplaceWith;
};

/** subst behaviour for changing the local variables to have unique
 * names.
 */
class LocalSymbolBehaviour : public SubstBehaviourBase
{
public:
    LocalSymbolBehaviour(LispStringPtr* aOriginalNames,
                         LispStringPtr* aNewNames, LispInt aNrNames);
    virtual LispBoolean Matches(LispPtr& aResult, LispPtr& aElement);
private:
    LispStringPtr* iOriginalNames;
    LispStringPtr* iNewNames;
    LispInt iNrNames;
};




#endif

