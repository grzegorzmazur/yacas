
#include "yacasprivate.h"
#include "substitute.h"
#include "lispatom.h"
#include "standard.h"
#include "lispeval.h"

SubstBehaviourBase::~SubstBehaviourBase()
{
}


//Subst, Substitute, FullSubstitute
void InternalSubstitute(LispPtr& aTarget, LispPtr& aSource,
                        SubstBehaviourBase& aBehaviour)
{
    LispObject* object = aSource;
    LISPASSERT(object);
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
        aResult = (iToReplaceWith->Copy());
        return true;
    }
    return false;
}

LocalSymbolBehaviour::LocalSymbolBehaviour(LispEnvironment& aEnvironment,LispString ** aOriginalNames,
                     LispString ** aNewNames, LispInt aNrNames)
: iEnvironment(aEnvironment),iOriginalNames(aOriginalNames),iNewNames(aNewNames), iNrNames(aNrNames)
{
}

bool LocalSymbolBehaviour::Matches(LispPtr& aResult, LispPtr& aElement)
{
    LispString * name = aElement->String();
    if (!name)
        return false;

    LispInt i;
    for (i=0;i<iNrNames;i++)
    {
        if (name == iOriginalNames[i])
        {
            aResult = (LispAtom::New(iEnvironment,iNewNames[i]->c_str()));
            DBG_( aResult->SetFileAndLine(aElement->iFileName,aElement->iLine); )
            return true;
        }
    }
    return false;
}

#define InternalEval iEnvironment.iEvaluator->Eval

bool BackQuoteBehaviour::Matches(LispPtr& aResult, LispPtr& aElement)
{
    if (!aElement->SubList()) return false;
    LispObject* ptr = (*aElement->SubList());
    if (!ptr) return false;
    if (!ptr->String()) return false;

    if (!std::strcmp("`", ptr->String()->c_str()))
    {
      aResult = (aElement);
      return true;
    }

    if (std::strcmp("@", ptr->String()->c_str()))
        return false;
    ptr = ptr->Nixed();
    if (!ptr) return false;
    if (ptr->String())
    {
        LispPtr cur(ptr);
/*
        LispPtr result;
        InternalEval(iEnvironment, result, cur);
        InternalSubstitute(aResult, result,*this);
*/
        InternalEval(iEnvironment, aResult, cur);
        return true;
    }
    else
    {
        ptr = (*ptr->SubList());
        LispPtr cur(ptr);
        LispPtr args(ptr->Nixed());
        LispPtr result;
        InternalEval(iEnvironment, result, cur);
        result->Nixed() = (args);
        LispPtr result2(LispSubList::New(result));
        InternalSubstitute(aResult, result2,*this);
        return true;
    }
    return false;
}

