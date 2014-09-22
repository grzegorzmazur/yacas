#include "yacas/yacasprivate.h"
#include "yacas/yacasbase.h"
#include "yacas/patterns.h"
#include "yacas/standard.h"
#include "yacas/mathuserfunc.h"
#include "yacas/lispobject.h"
#include "yacas/lispeval.h"
#include "yacas/standard.h"

#ifdef YACAS_DEBUG
#include <stdio.h>
#endif // YACAS_DEBUG

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
    if (!arguments[iVarIndex])
    {
        arguments[iVarIndex] = (aExpression);
        return true;
    }
    else
    {
        if (InternalEquals(aEnvironment, aExpression, arguments[iVarIndex]))
        {
            return true;
        }
        return false;
    }
    return false;
}

bool MatchSubList::ArgumentMatches(LispEnvironment& aEnvironment,
                                          LispPtr& aExpression,
                                          LispPtr* arguments) const
{
  if (!aExpression->SubList())
    return false;

  LispIterator iter(aExpression);
  LispObject * pObj = iter.getObj();

  if (!pObj)
      throw LispErrInvalidArg();

  LispPtr * pPtr = pObj->SubList();

  if (!pPtr)
      throw LispErrNotList();

  iter = *pPtr;

  const LispInt iNrMatchers = iMatchers.size();
  for (LispInt i=0;i<iNrMatchers;i++,++iter)
  {
    if (!iter.getObj())
      return false;
    if (!iMatchers[i]->ArgumentMatches(aEnvironment,*iter,arguments))
      return false;
  }
  if (iter.getObj())
    return false;
  return true;
}

LispInt YacasPatternPredicateBase::LookUp(LispString * aVariable)
{
    const std::size_t n = iVariables.size();
    for (std::size_t i = 0; i < n; ++i)
        if (iVariables[i] == aVariable)
            return i;

    aVariable->iReferenceCount += 1;
    iVariables.push_back(aVariable);
    return iVariables.size() - 1;
}


YacasParamMatcherBase* YacasPatternPredicateBase::MakeParamMatcher(LispEnvironment& aEnvironment, LispObject* aPattern)
{
    if (!aPattern)
        return nullptr;

    if (aPattern->Number(aEnvironment.Precision()))
    {
        return NEW MatchNumber(aPattern->Number(aEnvironment.Precision()));
    }

    // Deal with atoms
    if (aPattern->String())
    {
        return NEW MatchAtom(aPattern->String());
    }

    // Else it must be a sublist
    if (aPattern->SubList())
    {
        // See if it is a variable template:
        LispPtr* sublist = aPattern->SubList();
        assert(sublist);

        LispInt num = InternalListLength(*sublist);

        // variable matcher here...
        if (num>1)
        {
            LispObject* head = (*sublist);
            if (head->String() == aEnvironment.HashTable().LookUp("_"))
            {
                LispObject* second = head->Nixed();
                if (second->String())
                {
                    LispInt index = LookUp(second->String());

                    // Make a predicate for the type, if needed
                    if (num>2)
                    {
                        LispPtr third;

                        LispObject* predicate = second->Nixed();
                        if (predicate->SubList())
                        {
                            InternalFlatCopy(third, *predicate->SubList());
                        }
                        else
                        {
                            third = (second->Nixed()->Copy());
                        }

                        LispObject* last = third;
                        while (!!last->Nixed())
                            last = last->Nixed();

                        last->Nixed() = LispAtom::New(aEnvironment, *second->String());

                        LispPtr pred(LispSubList::New(third));
                        DBG_( third->Nixed()->SetFileAndLine(second->iFileName,second->iLine); )
                        DBG_( pred->SetFileAndLine(head->iFileName,head->iLine); )
                        iPredicates.push_back(pred);
                    }
                    return NEW MatchVariable(index);
                }
            }
        }

        std::vector<YacasParamMatcherBase*> matchers;
        matchers.reserve(num);
        LispIterator iter(*sublist);
        for (LispInt i = 0; i < num; ++i, ++iter) {
            matchers.push_back(MakeParamMatcher(aEnvironment,iter.getObj()));
            assert(matchers[i]);
        }
        return NEW MatchSubList(std::move(matchers));
    }

    return nullptr;
}

YacasPatternPredicateBase::YacasPatternPredicateBase(LispEnvironment& aEnvironment, LispPtr& aPattern,
                                                     LispPtr& aPostPredicate)
  : iParamMatchers(),iVariables(),iPredicates()
{
    LispIterator iter(aPattern);
    for ( ; iter.getObj(); ++iter)
  {
        YacasParamMatcherBase* matcher = MakeParamMatcher(aEnvironment,iter.getObj());
        assert(matcher!=nullptr);
        iParamMatchers.push_back(matcher);
  }
  LispPtr post(aPostPredicate);
  iPredicates.push_back(post);
}

bool YacasPatternPredicateBase::Matches(LispEnvironment& aEnvironment,
                                              LispPtr& aArguments)
{
    LispPtr* arguments = nullptr;
    if (!iVariables.empty())
        arguments = NEW LispPtr[iVariables.size()];
    LocalArgs args(arguments); //Deal with destruction
    LispIterator iter(aArguments);
    const std::size_t n = iParamMatchers.size();
    for (std::size_t i = 0; i < n; ++i, ++iter)
  {
        if (!iter.getObj())
            return false;
        if (!iParamMatchers[i]->ArgumentMatches(aEnvironment,*iter,arguments))
        {
            return false;
        }
  }
    if (iter.getObj())
        return false;

    {
        // set the local variables.
        LispLocalFrame frame(aEnvironment,false);

        SetPatternVariables(aEnvironment,arguments);

        // do the predicates
        if (!CheckPredicates(aEnvironment))
            return false;
    }

    // set the local variables for sure now
    SetPatternVariables(aEnvironment,arguments);

    return true;
}




bool YacasPatternPredicateBase::Matches(LispEnvironment& aEnvironment,
                                              LispPtr* aArguments)
{
    LispPtr* arguments = nullptr;
    if (!iVariables.empty())
        arguments = NEW LispPtr[iVariables.size()];
    LocalArgs args(arguments); //Deal with destruction

    const std::size_t n = iParamMatchers.size();
    for (std::size_t i = 0; i < n; ++i)
    {
        if (!iParamMatchers[i]->ArgumentMatches(aEnvironment,aArguments[i],arguments))
        {
            return false;
        }
    }

    {
        // set the local variables.
        LispLocalFrame frame(aEnvironment,false);
        SetPatternVariables(aEnvironment,arguments);

        // do the predicates
        if (!CheckPredicates(aEnvironment))
            return false;
    }

    // set the local variables for sure now
    SetPatternVariables(aEnvironment,arguments);
    return true;
}



bool YacasPatternPredicateBase::CheckPredicates(LispEnvironment& aEnvironment)
{
    const std::size_t n = iPredicates.size();
  for (std::size_t i = 0; i < n; ++i)
  {
    LispPtr pred;
    aEnvironment.iEvaluator->Eval(aEnvironment, pred, iPredicates[i]);
    if (IsFalse(aEnvironment, pred))
    {
      return false;
    }
    // If the result is not False, it should be True, else probably something is wrong (the expression returned unevaluated)
    bool isTrue = IsTrue(aEnvironment, pred);
    if (!isTrue)
    {
#define LIM_AL 60
      LispString strout;

#ifdef YACAS_DEBUG
        if (iPredicates[i]->iFileName)
        {
          printf("File %s, line %d\n",iPredicates[i]->iFileName, iPredicates[i]->iLine);
        }
#endif


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


void YacasPatternPredicateBase::SetPatternVariables(LispEnvironment& aEnvironment,
                                                    LispPtr* arguments)
{
    const std::size_t n = iVariables.size();
    for (std::size_t i = 0; i < n; ++i)
        aEnvironment.NewLocal(iVariables[i],arguments[i]);
}


YacasPatternPredicateBase::~YacasPatternPredicateBase()
{
    for (LispString* p: iVariables)
        if (--p->iReferenceCount == 0)
            delete p;

    for (YacasParamMatcherBase* p: iParamMatchers)
        delete p;
}
