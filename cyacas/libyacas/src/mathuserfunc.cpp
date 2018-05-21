#include "yacas/mathuserfunc.h"
#include "yacas/lispeval.h"
#include "yacas/lispobject.h"
#include "yacas/patternclass.h"
#include "yacas/patterns.h"
#include "yacas/standard.h"
#include "yacas/substitute.h"

#include <memory>

#define InternalEval aEnvironment.iEvaluator->Eval

bool BranchingUserFunction::BranchRule::Matches(LispEnvironment& aEnvironment,
                                                LispPtr* aArguments)
{
    LispPtr pred;
    InternalEval(aEnvironment, pred, iPredicate);
    return IsTrue(aEnvironment, pred);
}
int BranchingUserFunction::BranchRule::Precedence() const
{
    return iPrecedence;
}
LispPtr& BranchingUserFunction::BranchRule::Body()
{
    return iBody;
}

bool BranchingUserFunction::BranchRuleTruePredicate::Matches(
    LispEnvironment& aEnvironment, LispPtr* aArguments)
{
    return true;
}

bool BranchingUserFunction::BranchPattern::Matches(
    LispEnvironment& aEnvironment, LispPtr* aArguments)
{
    return iPatternClass->Matches(aEnvironment, aArguments);
}
int BranchingUserFunction::BranchPattern::Precedence() const
{
    return iPrecedence;
}
LispPtr& BranchingUserFunction::BranchPattern::Body()
{
    return iBody;
}

BranchingUserFunction::BranchingUserFunction(LispPtr& aParameters) :
    iParameters(),
    iRules(),
    iParamList(aParameters)
{
    for (LispIterator iter(aParameters); iter.getObj(); ++iter) {
        if (!iter.getObj()->String())
            throw LispErrCreatingUserFunction();

        BranchParameter param(iter.getObj()->String());
        iParameters.push_back(param);
    }
}

BranchingUserFunction::~BranchingUserFunction()
{
    for (BranchRuleBase* p : iRules)
        delete p;
}

void BranchingUserFunction::Evaluate(LispPtr& aResult,
                                     LispEnvironment& aEnvironment,
                                     LispPtr& aArguments) const
{
    const int arity = Arity();
    int i;

    if (Traced()) {
        LispPtr tr(LispSubList::New(aArguments));
        TraceShowEnter(aEnvironment, tr);
        tr = nullptr;
    }

    LispIterator iter(aArguments);
    ++iter;

    // unrollable arguments
    std::unique_ptr<LispPtr[]> arguments(arity == 0 ? nullptr
                                                    : new LispPtr[arity]);

    // Walk over all arguments, evaluating them as necessary
    for (i = 0; i < arity; i++, ++iter) {
        if (!iter.getObj())
            throw LispErrWrongNumberOfArgs();

        if (iParameters[i].iHold) {
            arguments[i] = iter.getObj()->Copy();
        } else {
            // Check(iter.getObj(), KLispErrWrongNumberOfArgs);  // checked
            // above
            InternalEval(aEnvironment, arguments[i], *iter);
        }
    }

    if (Traced()) {
        LispIterator iter(aArguments);
        for (i = 0; i < arity; i++)
            TraceShowArg(aEnvironment, *++iter, arguments[i]);
    }

    // declare a new local stack.
    LispLocalFrame frame(aEnvironment, Fenced());

    // define the local variables.
    for (i = 0; i < arity; i++) {
        const LispString* variable = iParameters[i].iParameter;
        // set the variable to the new value
        aEnvironment.NewLocal(variable, arguments[i]);
    }

    // walk the rules database, returning the evaluated result if the
    // predicate is true.
    const std::size_t nrRules = iRules.size();
    UserStackInformation& st = aEnvironment.iEvaluator->StackInformation();
    for (std::size_t i = 0; i < nrRules; i++) {
        BranchRuleBase* thisRule = iRules[i];
        assert(thisRule);

        st.iRulePrecedence = thisRule->Precedence();
        bool matches = thisRule->Matches(aEnvironment, arguments.get());
        if (matches) {
            st.iSide = 1;
            InternalEval(aEnvironment, aResult, thisRule->Body());
            goto FINISH;
        }

        // If rules got inserted, walk back
        while (thisRule != iRules[i] && i > 0)
            i--;
    }

    // No predicate was true: return a new expression with the evaluated
    // arguments.

    {
        LispPtr full(aArguments->Copy());
        if (arity == 0) {
            full->Nixed() = nullptr;
        } else {
            full->Nixed() = arguments[0];
            for (i = 0; i < arity - 1; i++)
                arguments[i]->Nixed() = arguments[i + 1];
        }
        aResult = LispSubList::New(full);
    }

FINISH:
    if (Traced()) {
        LispPtr tr(LispSubList::New(aArguments));
        TraceShowLeave(aEnvironment, aResult, tr);
        tr = nullptr;
    }
}

void BranchingUserFunction::HoldArgument(const LispString* aVariable)
{
    const std::size_t nrc = iParameters.size();
    for (std::size_t i = 0; i < nrc; ++i) {
        if (iParameters[i].iParameter == aVariable)
            iParameters[i].iHold = true;
    }
}

int BranchingUserFunction::Arity() const
{
    return iParameters.size();
}

bool BranchingUserFunction::IsArity(int aArity) const
{
    return Arity() == aArity;
}

void BranchingUserFunction::DeclareRule(int aPrecedence,
                                        LispPtr& aPredicate,
                                        LispPtr& aBody)
{
    // New branching rule.
    BranchRule* newRule = new BranchRule(aPrecedence, aPredicate, aBody);

    if (!newRule)
        throw LispErrCreatingRule();

    InsertRule(aPrecedence, newRule);
}

void BranchingUserFunction::DeclareRule(int aPrecedence, LispPtr& aBody)
{
    // New branching rule.
    BranchRule* newRule = new BranchRuleTruePredicate(aPrecedence, aBody);

    if (!newRule)
        throw LispErrCreatingRule();

    InsertRule(aPrecedence, newRule);
}

void BranchingUserFunction::DeclarePattern(int aPrecedence,
                                           LispPtr& aPredicate,
                                           LispPtr& aBody)
{
    // New branching rule.
    BranchPattern* newRule = new BranchPattern(aPrecedence, aPredicate, aBody);

    if (!newRule)
        throw LispErrCreatingRule();

    InsertRule(aPrecedence, newRule);
}

void BranchingUserFunction::InsertRule(int aPrecedence, BranchRuleBase* newRule)
{
    // Find place to insert
    int low, high, mid;
    low = 0;
    high = iRules.size();

    // Constant time: find out if the precedence is before any of the
    // currently defined rules or past them.
    if (high > 0) {
        if (iRules[0]->Precedence() > aPrecedence) {
            mid = 0;
            goto CONTINUE;
        }
        if (iRules[high - 1]->Precedence() < aPrecedence) {
            mid = high;
            goto CONTINUE;
        }
    }

    // Otherwise, O(log n) search algorithm for place to insert
    for (;;) {
        if (low >= high) {
            mid = low;
            goto CONTINUE;
        }
        mid = (low + high) >> 1;

        if (iRules[mid]->Precedence() > aPrecedence) {
            high = mid;
        } else if (iRules[mid]->Precedence() < aPrecedence) {
            low = (++mid);
        } else {
            goto CONTINUE;
        }
    }
CONTINUE:
    // Insert it
    iRules.insert(iRules.begin() + mid, newRule);
}

const LispPtr& BranchingUserFunction::ArgList() const
{
    return iParamList;
}

ListedBranchingUserFunction::ListedBranchingUserFunction(LispPtr& aParameters) :
    BranchingUserFunction(aParameters)
{
}

bool ListedBranchingUserFunction::IsArity(int aArity) const
{
    // nr arguments handled is bound by a minimum: the number of arguments
    // to this function.
    return Arity() <= aArity;
}

void ListedBranchingUserFunction::Evaluate(LispPtr& aResult,
                                           LispEnvironment& aEnvironment,
                                           LispPtr& aArguments) const
{
    LispPtr newArgs;
    LispIterator iter(aArguments);
    LispPtr* ptr = &newArgs;
    const int arity = Arity();
    // Make a copy of the arguments first
    // TODO: if we were to change the internal representation to a cons cell,
    // this copying would not be needed
    for (int i = 0; i < arity && iter.getObj(); ++i, ++iter) {
        *ptr = iter.getObj()->Copy();
        ptr = &((*ptr)->Nixed());
    }

    if (!iter.getObj()->Nixed()) {
        (*ptr) = (iter.getObj()->Copy());
        ++iter;
        assert(!iter.getObj());
    } else {
        LispPtr head(aEnvironment.iList->Copy());
        head->Nixed() = iter.getObj();
        *ptr = (LispSubList::New(head));
    }
    BranchingUserFunction::Evaluate(aResult, aEnvironment, newArgs);
}

MacroUserFunction::MacroUserFunction(LispPtr& aParameters) :
    BranchingUserFunction(aParameters)
{
    LispIterator iter(aParameters);
    for (int i = 0; iter.getObj(); i++, ++iter) {
        if (!iter.getObj()->String())
            throw LispErrCreatingUserFunction();

        iParameters[i].iHold = true;
    }
    UnFence();
}

void MacroUserFunction::Evaluate(LispPtr& aResult,
                                 LispEnvironment& aEnvironment,
                                 LispPtr& aArguments) const
{
    const int arity = Arity();
    int i;

    if (Traced()) {
        LispPtr tr(LispSubList::New(aArguments));
        TraceShowEnter(aEnvironment, tr);
        tr = (nullptr);
    }

    LispIterator iter(aArguments);
    ++iter;

    // unrollable arguments
    std::unique_ptr<LispPtr[]> arguments(arity == 0 ? nullptr
                                                    : new LispPtr[arity]);

    // Walk over all arguments, evaluating them as necessary
    for (i = 0; i < arity; i++, ++iter) {
        if (!iter.getObj())
            throw LispErrWrongNumberOfArgs();

        if (iParameters[i].iHold)
            arguments[i] = iter.getObj()->Copy();
        else
            InternalEval(aEnvironment, arguments[i], *iter);
    }

    if (Traced()) {
        LispIterator iter(aArguments);
        // TODO: ideally we would only need an iterator here
        ++iter;
        for (i = 0; i < arity; i++) {
            TraceShowArg(aEnvironment, *iter, arguments[i]);
            ++iter;
        }
    }

    LispPtr substedBody;
    {
        // declare a new local stack.
        LispLocalFrame frame(aEnvironment, false);

        // define the local variables.
        for (i = 0; i < arity; i++) {
            const LispString* variable = iParameters[i].iParameter;
            // set the variable to the new value
            aEnvironment.NewLocal(variable, arguments[i]);
        }

        // walk the rules database, returning the evaluated result if the
        // predicate is true.
        const std::size_t nrRules = iRules.size();
        UserStackInformation& st = aEnvironment.iEvaluator->StackInformation();
        for (std::size_t i = 0; i < nrRules; i++) {
            BranchRuleBase* thisRule = iRules[i];
            assert(thisRule);

            st.iRulePrecedence = thisRule->Precedence();
            const bool matches =
                thisRule->Matches(aEnvironment, arguments.get());
            if (matches) {
                st.iSide = 1;

                BackQuoteBehaviour behaviour(aEnvironment);
                InternalSubstitute(substedBody, thisRule->Body(), behaviour);
                break;
            }

            // If rules got inserted, walk back
            while (thisRule != iRules[i] && i > 0)
                i--;
        }
    }

    if (!!substedBody) {
        InternalEval(aEnvironment, aResult, substedBody);
    } else
    // No predicate was true: return a new expression with the evaluated
    // arguments.
    {
        LispPtr full(aArguments->Copy());
        if (arity == 0) {
            full->Nixed() = nullptr;
        } else {
            full->Nixed() = arguments[0];
            for (i = 0; i < arity - 1; i++) {
                arguments[i]->Nixed() = arguments[i + 1];
            }
        }
        aResult = LispSubList::New(full);
    }
    if (Traced()) {
        LispPtr tr(LispSubList::New(aArguments));
        TraceShowLeave(aEnvironment, aResult, tr);
        tr = nullptr;
    }
}

ListedMacroUserFunction::ListedMacroUserFunction(LispPtr& aParameters) :
    MacroUserFunction(aParameters)
{
}

bool ListedMacroUserFunction::IsArity(int aArity) const
{
    return Arity() <= aArity;
}

void ListedMacroUserFunction::Evaluate(LispPtr& aResult,
                                       LispEnvironment& aEnvironment,
                                       LispPtr& aArguments) const
{
    LispPtr newArgs;
    LispIterator iter(aArguments);
    LispPtr* ptr = &newArgs;
    const int arity = Arity();
    // TODO: the code would look a lot easier if we could do with only an
    // iterator
    for (int i = 0; i < arity && iter.getObj(); ++i, ++iter) {
        *ptr = (*iter)->Copy();
        ptr = &((*ptr)->Nixed());
    }

    if (!iter.getObj()->Nixed()) {
        *ptr = iter.getObj()->Copy();
        (*ptr)->Nixed();
        ++iter;
        assert(!iter.getObj());
    } else {
        LispPtr head(aEnvironment.iList->Copy());
        head->Nixed() = iter.getObj();
        *ptr = LispSubList::New(head);
    }

    MacroUserFunction::Evaluate(aResult, aEnvironment, newArgs);
}
