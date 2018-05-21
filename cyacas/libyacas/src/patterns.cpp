#include "yacas/patterns.h"
#include "yacas/lispeval.h"
#include "yacas/lispobject.h"
#include "yacas/mathuserfunc.h"
#include "yacas/standard.h"

#include <memory>

bool MatchAtom::ArgumentMatches(LispEnvironment& aEnvironment,
                                LispPtr& aExpression,
                                LispPtr* arguments) const
{
    // If it is a floating point, don't even bother comparing
    if (!!aExpression)
        if (aExpression->Number(0))
            if (!aExpression->Number(0)->IsInt())
                return false;

    return (iString == aExpression->String());
}

bool MatchNumber::ArgumentMatches(LispEnvironment& aEnvironment,
                                  LispPtr& aExpression,
                                  LispPtr* arguments) const
{
    if (aExpression->Number(aEnvironment.Precision()))
        return iNumber->Equals(*aExpression->Number(aEnvironment.Precision()));

    return false;
}

bool MatchVariable::ArgumentMatches(LispEnvironment& aEnvironment,
                                    LispPtr& aExpression,
                                    LispPtr* arguments) const
{
    if (!arguments[iVarIndex]) {
        arguments[iVarIndex] = aExpression;
        return true;
    }

    if (InternalEquals(aEnvironment, aExpression, arguments[iVarIndex]))
        return true;

    return false;
}

bool MatchSubList::ArgumentMatches(LispEnvironment& aEnvironment,
                                   LispPtr& aExpression,
                                   LispPtr* arguments) const
{
    if (!aExpression->SubList())
        return false;

    LispIterator iter(aExpression);
    LispObject* pObj = iter.getObj();

    if (!pObj)
        throw LispErrInvalidArg();

    LispPtr* pPtr = pObj->SubList();

    if (!pPtr)
        throw LispErrNotList();

    iter = *pPtr;

    const int iNrMatchers = iMatchers.size();
    for (int i = 0; i < iNrMatchers; i++, ++iter) {
        if (!iter.getObj())
            return false;
        if (!iMatchers[i]->ArgumentMatches(aEnvironment, *iter, arguments))
            return false;
    }
    if (iter.getObj())
        return false;
    return true;
}

int YacasPatternPredicateBase::LookUp(const LispString* aVariable)
{
    const std::size_t n = iVariables.size();
    for (std::size_t i = 0; i < n; ++i)
        if (iVariables[i] == aVariable)
            return i;

    iVariables.push_back(aVariable);
    return iVariables.size() - 1;
}

const YacasParamMatcherBase*
YacasPatternPredicateBase::MakeParamMatcher(LispEnvironment& aEnvironment,
                                            LispObject* aPattern)
{
    if (!aPattern)
        return nullptr;

    if (aPattern->Number(aEnvironment.Precision()))
        return new MatchNumber(aPattern->Number(aEnvironment.Precision()));

    // Deal with atoms
    if (aPattern->String())
        return new MatchAtom(aPattern->String());

    // Else it must be a sublist
    if (aPattern->SubList()) {
        // See if it is a variable template:
        LispPtr* sublist = aPattern->SubList();
        assert(sublist);

        int num = InternalListLength(*sublist);

        // variable matcher here...
        if (num > 1) {
            LispObject* head = (*sublist);
            if (head->String() == aEnvironment.HashTable().LookUp("_")) {
                LispObject* second = head->Nixed();
                if (second->String()) {
                    int index = LookUp(second->String());

                    // Make a predicate for the type, if needed
                    if (num > 2) {
                        LispPtr third;

                        LispObject* predicate = second->Nixed();
                        if (predicate->SubList()) {
                            InternalFlatCopy(third, *predicate->SubList());
                        } else {
                            third = (second->Nixed()->Copy());
                        }

                        LispObject* last = third;
                        while (!!last->Nixed())
                            last = last->Nixed();

                        last->Nixed() =
                            LispAtom::New(aEnvironment, *second->String());

                        iPredicates.push_back(LispPtr(LispSubList::New(third)));
                    }
                    return new MatchVariable(index);
                }
            }
        }

        std::vector<const YacasParamMatcherBase*> matchers;
        matchers.reserve(num);
        LispIterator iter(*sublist);
        for (int i = 0; i < num; ++i, ++iter) {
            matchers.push_back(MakeParamMatcher(aEnvironment, iter.getObj()));
            assert(matchers[i]);
        }
        return new MatchSubList(std::move(matchers));
    }

    return nullptr;
}

YacasPatternPredicateBase::YacasPatternPredicateBase(
    LispEnvironment& aEnvironment, LispPtr& aPattern, LispPtr& aPostPredicate)
{
    for (LispIterator iter(aPattern); iter.getObj(); ++iter) {
        const YacasParamMatcherBase* matcher =
            MakeParamMatcher(aEnvironment, iter.getObj());
        assert(matcher != nullptr);
        iParamMatchers.push_back(matcher);
    }

    iPredicates.push_back(aPostPredicate);
}

bool YacasPatternPredicateBase::Matches(LispEnvironment& aEnvironment,
                                        LispPtr& aArguments)
{
    std::unique_ptr<LispPtr[]> arguments(
        iVariables.empty() ? nullptr : new LispPtr[iVariables.size()]);

    LispIterator iter(aArguments);
    const std::size_t n = iParamMatchers.size();

    for (std::size_t i = 0; i < n; ++i, ++iter) {

        if (!iter.getObj())
            return false;

        if (!iParamMatchers[i]->ArgumentMatches(
                aEnvironment, *iter, arguments.get()))
            return false;
    }

    if (iter.getObj())
        return false;

    {
        // set the local variables.
        LispLocalFrame frame(aEnvironment, false);

        SetPatternVariables(aEnvironment, arguments.get());

        // do the predicates
        if (!CheckPredicates(aEnvironment))
            return false;
    }

    // set the local variables for sure now
    SetPatternVariables(aEnvironment, arguments.get());

    return true;
}

bool YacasPatternPredicateBase::Matches(LispEnvironment& aEnvironment,
                                        LispPtr* aArguments)
{
    std::unique_ptr<LispPtr[]> arguments(
        iVariables.empty() ? nullptr : new LispPtr[iVariables.size()]);

    const std::size_t n = iParamMatchers.size();
    for (std::size_t i = 0; i < n; ++i)
        if (!iParamMatchers[i]->ArgumentMatches(
                aEnvironment, aArguments[i], arguments.get()))
            return false;

    {
        // set the local variables.
        LispLocalFrame frame(aEnvironment, false);
        SetPatternVariables(aEnvironment, arguments.get());

        // do the predicates
        if (!CheckPredicates(aEnvironment))
            return false;
    }

    // set the local variables for sure now
    SetPatternVariables(aEnvironment, arguments.get());

    return true;
}

bool YacasPatternPredicateBase::CheckPredicates(LispEnvironment& aEnvironment)
{
    const std::size_t n = iPredicates.size();
    for (std::size_t i = 0; i < n; ++i) {
        LispPtr pred;
        aEnvironment.iEvaluator->Eval(aEnvironment, pred, iPredicates[i]);
        if (IsFalse(aEnvironment, pred)) {
            return false;
        }
        // If the result is not False, it should be True, else probably
        // something is wrong (the expression returned unevaluated)
        bool isTrue = IsTrue(aEnvironment, pred);
        if (!isTrue) {
#define LIM_AL 60
            LispString strout;

            aEnvironment.iErrorOutput << "The predicate\n\t";
            PrintExpression(strout, iPredicates[i], aEnvironment, LIM_AL);
            aEnvironment.iErrorOutput << strout;
            aEnvironment.iErrorOutput << "\nevaluated to\n\t";
            PrintExpression(strout, pred, aEnvironment, LIM_AL);
            aEnvironment.iErrorOutput << strout << '\n';

            ShowStack(aEnvironment);
            throw LispErrMaxRecurseDepthReached();
        }
    }
    return true;
}

void YacasPatternPredicateBase::SetPatternVariables(
    LispEnvironment& aEnvironment, LispPtr* arguments)
{
    const std::size_t n = iVariables.size();
    for (std::size_t i = 0; i < n; ++i)
        aEnvironment.NewLocal(iVariables[i], arguments[i]);
}

YacasPatternPredicateBase::~YacasPatternPredicateBase()
{
    for (const YacasParamMatcherBase* p : iParamMatchers)
        delete p;
}
