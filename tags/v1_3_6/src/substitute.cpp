
#include "yacas/yacasprivate.h"
#include "yacas/substitute.h"
#include "yacas/lispatom.h"
#include "yacas/standard.h"
#include "yacas/lispeval.h"

//Subst, Substitute, FullSubstitute
void InternalSubstitute(LispPtr& aTarget, LispPtr& aSource,
                        SubstBehaviourBase& aBehaviour)
{
    LispObject* object = aSource;
    assert(object);
    if (!aBehaviour.Matches(aTarget,aSource))
    {
        LispPtr* oldList = object->SubList();
        if (oldList)
        {
            LispPtr newList;
            LispPtr* next = &newList;
            while (!!(*oldList))
            {
                InternalSubstitute(*next, *oldList, aBehaviour);
                oldList = &(*oldList)->Nixed();
                next = &(*next)->Nixed();
            }

            aTarget = (LispSubList::New(newList));
            DBG_( aTarget->SetFileAndLine(object->iFileName,object->iLine); )
        }
        else
        {
            aTarget = (object->Copy());
        }
    }
}

SubstBehaviour::SubstBehaviour(LispEnvironment& aEnvironment,
                               LispPtr& aToMatch,
                               LispPtr& aToReplaceWith)
: iEnvironment(aEnvironment), iToMatch(aToMatch),
  iToReplaceWith(aToReplaceWith)
{
}

bool SubstBehaviour::Matches(LispPtr& aResult, LispPtr& aElement)
{
    if (InternalEquals(iEnvironment, aElement, iToMatch))
    {
        aResult = iToReplaceWith->Copy();
        return true;
    }
    return false;
}

LocalSymbolBehaviour::LocalSymbolBehaviour(
    LispEnvironment& aEnvironment,
    const std::vector<const LispString*>&& aOriginalNames,
    const std::vector<const LispString*>&& aNewNames):
    iEnvironment(aEnvironment),
    iOriginalNames(aOriginalNames),
    iNewNames(aNewNames)
{
}

bool LocalSymbolBehaviour::Matches(LispPtr& aResult, LispPtr& aElement)
{
    const LispString* name = aElement->String();
    if (!name)
        return false;

    const LispInt iNrNames = iOriginalNames.size();
    for (LispInt i=0;i<iNrNames;i++)
    {
        if (name == iOriginalNames[i])
        {
            aResult = LispAtom::New(iEnvironment, *iNewNames[i]);
            DBG_( aResult->SetFileAndLine(aElement->iFileName,aElement->iLine); )
            return true;
        }
    }
    return false;
}

bool BackQuoteBehaviour::Matches(LispPtr& aResult, LispPtr& aElement)
{
    if (!aElement->SubList()) return false;
    LispObject* ptr = (*aElement->SubList());
    if (!ptr) return false;
    if (!ptr->String()) return false;

    if (*ptr->String() == "`")
    {
      aResult = (aElement);
      return true;
    }

    if (*ptr->String() != "@")
        return false;

    ptr = ptr->Nixed();
    if (!ptr)
        return false;

    if (ptr->String())
    {
        LispPtr cur(ptr);
/*
        LispPtr result;
        iEnvironment.iEvaluator->Eval(iEnvironment, result, cur);
        InternalSubstitute(aResult, result,*this);
*/
        iEnvironment.iEvaluator->Eval(iEnvironment, aResult, cur);
        return true;
    }
    else
    {
        ptr = (*ptr->SubList());
        LispPtr cur(ptr);
        LispPtr args(ptr->Nixed());
        LispPtr result;
        iEnvironment.iEvaluator->Eval(iEnvironment, result, cur);
        result->Nixed() = (args);
        LispPtr result2(LispSubList::New(result));
        InternalSubstitute(aResult, result2,*this);
        return true;
    }
    return false;
}

