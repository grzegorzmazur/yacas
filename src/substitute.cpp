
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
    LispObject* object = aSource.Get();
    LISPASSERT(object != NULL);
    if (!aBehaviour.Matches(aTarget,aSource))
    {
        LispPtr* oldList = object->SubList();
        if (oldList != NULL)
        {
            LispPtr newList;
            LispPtr* next = &newList;
            while (oldList->Get() != NULL)
            {
                InternalSubstitute(*next, *oldList, aBehaviour);
                oldList = &oldList->Get()->Next();
                next = &next->Get()->Next();
            }

            aTarget.Set(LispSubList::New(newList.Get()));
#ifdef YACAS_DEBUG
            aTarget.Get()->SetFileAndLine(object->iFileName,object->iLine);
#endif
        }
        else
        {
            aTarget.Set(object->Copy(LispFalse));
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

LispBoolean SubstBehaviour::Matches(LispPtr& aResult, LispPtr& aElement)
{
    if (InternalEquals(iEnvironment, aElement, iToMatch))
    {
        aResult.Set(iToReplaceWith.Get()->Copy(LispFalse));
        return LispTrue;
    }
    return LispFalse;
}

LocalSymbolBehaviour::LocalSymbolBehaviour(LispStringPtr* aOriginalNames,
                     LispStringPtr* aNewNames, LispInt aNrNames)
: iOriginalNames(aOriginalNames),iNewNames(aNewNames), iNrNames(aNrNames)
{
}

LispBoolean LocalSymbolBehaviour::Matches(LispPtr& aResult, LispPtr& aElement)
{
    LispStringPtr name = aElement.Get()->String();
    if (name == NULL)
        return LispFalse;

    LispInt i;
    for (i=0;i<iNrNames;i++)
    {
        if (name == iOriginalNames[i])
        {
            aResult.Set(LispAtom::New(iNewNames[i]));
#ifdef YACAS_DEBUG
            aResult.Get()->SetFileAndLine(aElement.Get()->iFileName,aElement.Get()->iLine);
#endif
            return LispTrue;
        }
    }
    return LispFalse;
}

#define InternalEval iEnvironment.iEvaluator->Eval

LispBoolean BackQuoteBehaviour::Matches(LispPtr& aResult, LispPtr& aElement)
{
    if (!aElement.Get()->SubList()) return LispFalse;
    LispObject* ptr = aElement.Get()->SubList()->Get();
    if (!ptr) return LispFalse;
    if (!ptr->String()) return LispFalse;
    if (!StrEqual("@", ptr->String()->String())) return LispFalse;
    ptr = ptr->Next().Get();
    if (!ptr) return LispFalse;
    if (ptr->String())
    {
        LispPtr cur;
        cur.Set(ptr);
/*
        LispPtr result;
        InternalEval(iEnvironment, result, cur);
        InternalSubstitute(aResult, result,*this);
*/

        InternalEval(iEnvironment, aResult, cur);

        return LispTrue;
    }
    else
    {
        ptr = ptr->SubList()->Get();
        LispPtr cur;
        cur.Set(ptr);
        LispPtr args;
        args.Set(ptr->Next().Get());
        LispPtr result;
        InternalEval(iEnvironment, result, cur);
        result.Get()->Next().Set(args.Get());
        LispPtr result2;
        result2.Set(LispSubList::New(result.Get()));
        InternalSubstitute(aResult, result2,*this);
        return LispTrue;
    }
    return LispFalse;
}


