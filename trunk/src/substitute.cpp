
#include "substitute.h"
#include "lispatom.h"
#include "standard.h"

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
#ifdef DEBUG_MODE
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
#ifdef DEBUG_MODE
            aResult.Get()->SetFileAndLine(aElement.Get()->iFileName,aElement.Get()->iLine);
#endif
            return LispTrue;
        }
    }
    return LispFalse;
}



