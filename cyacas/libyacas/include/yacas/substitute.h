#ifndef YACAS_SUBSTITUTE_H
#define YACAS_SUBSTITUTE_H

#include "lispobject.h"
#include "lispenvironment.h"

#include <vector>

/** Behaviour for substituting sub-expressions.
 */
class SubstBehaviourBase {
public:
    virtual ~SubstBehaviourBase() = default;
    virtual bool Matches(LispPtr& aResult, LispPtr& aElement) = 0;
};

/** main routine that can perform substituting of expressions
 */
void InternalSubstitute(LispPtr& aTarget, LispPtr& aSource,
                        SubstBehaviourBase& aBehaviour);


/** Substing one expression for another. The simplest form
 * of substitution
 */
class SubstBehaviour final: public SubstBehaviourBase
{
public:
    SubstBehaviour(LispEnvironment& aEnvironment,LispPtr& aToMatch,
                  LispPtr& aToReplaceWith);
    bool Matches(LispPtr& aResult, LispPtr& aElement) override;
private:
    LispEnvironment& iEnvironment;
    LispPtr& iToMatch;
    LispPtr& iToReplaceWith;
};

/** subst behaviour for changing the local variables to have unique
 * names.
 */
class LocalSymbolBehaviour final: public SubstBehaviourBase
{
public:
    LocalSymbolBehaviour(
        LispEnvironment& aEnvironment,
        const std::vector<const LispString*>&& aOriginalNames,
        const std::vector<const LispString*>&& aNewNames);

    bool Matches(LispPtr& aResult, LispPtr& aElement) override;

private:
    LispEnvironment& iEnvironment;
    std::vector<const LispString*> iOriginalNames;
    std::vector<const LispString*> iNewNames;
};

/** subst behaviour for backquote mechanism as in LISP.
 * When typing `(...) all occurrences of @a will be
 * replaced with:
 * 1) a evaluated if a is an atom
 * 2) function call with function name replaced by evaluated
 *    head of function if a is a function. For instance, if
 *    a is f(x) and f is g, then f(x) gets replaced by g(x)
 */
class BackQuoteBehaviour final: public SubstBehaviourBase
{
public:
    BackQuoteBehaviour(LispEnvironment& aEnvironment)
        : iEnvironment(aEnvironment) {};
    bool Matches(LispPtr& aResult, LispPtr& aElement) override;
    
private:
    LispEnvironment& iEnvironment;
};




#endif

