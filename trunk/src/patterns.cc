/*TODO
 - some bugs left:
   - - - a not working correctly...
 - make a TryRetractRule with a predicate as argument.
 */
#include "patterns.h"
#include "standard.h"
#include "mathuserfunc.h"
#include "lispobject.h"
#include "lispeval.h"
#include "standard.h"

#define YACAS_LOGGING
#include "log.h"
/*
*/

#define InternalEval aEnvironment.iEvaluator->Eval


YacasParamMatcherBase::~YacasParamMatcherBase()
{
}


MatchAtom::MatchAtom(LispStringPtr aString) : iString(aString)
{
}

LispBoolean MatchAtom::ArgumentMatches(LispEnvironment& aEnvironment,
                                       LispPtr& aExpression,
                                       LispPtr* arguments)
{
    /*
     LogPrintf("Enter match atom\n");
    if (aExpression.Get()->String())
        LogPrintf("Atom match %s to %s\n",
                  iString->String(),aExpression.Get()->String()->String());
    else
    {
        LogPrintf("trying to match atom to list\n");
    }
    */
    return (iString == aExpression.Get()->String());
}


MatchVariable::MatchVariable(LispInt aVarIndex) : iVarIndex(aVarIndex)
{
}
LispBoolean MatchVariable::ArgumentMatches(LispEnvironment& aEnvironment,
                                       LispPtr& aExpression,
                                       LispPtr* arguments)
{
    if (arguments[iVarIndex].Get() == NULL)
    {
        arguments[iVarIndex].Set(aExpression.Get());
//        LogPrintf("Set var %d\n",iVarIndex);
        return LispTrue;
    }
    else
    {
        if (InternalEquals(aEnvironment, aExpression, arguments[iVarIndex]))
        {
//            LogPrintf("Matched var %d\n",iVarIndex);
            return LispTrue;
        }
        return LispFalse;
    }
    return LispFalse;
}

MatchSubList::MatchSubList(YacasParamMatcherBase** aMatchers,
                           LispInt aNrMatchers)
: iMatchers(aMatchers),iNrMatchers(aNrMatchers)

{
}

MatchSubList::~MatchSubList()
{
    if (iMatchers)
    {
        LispInt i;
        for (i=0;i<iNrMatchers;i++)
            delete iMatchers[i];
        delete[] iMatchers;
    }
}

LispBoolean MatchSubList::ArgumentMatches(LispEnvironment& aEnvironment,
                                          LispPtr& aExpression,
                                          LispPtr* arguments)
{
    if (aExpression.Get()->SubList() == NULL)
        return LispFalse;
    LispInt i;

    LispIterator iter(aExpression);
    iter.GoSub();
    
    for (i=0;i<iNrMatchers;i++)
    {
        LispPtr* ptr = iter.Ptr();
        if (ptr == NULL)
            return LispFalse;
        if (iter() == NULL)
            return LispFalse;
        if (!iMatchers[i]->ArgumentMatches(aEnvironment,*ptr,arguments))
            return LispFalse;
        iter.GoNext();
    }
    if (iter() != NULL)
        return LispFalse;
    return LispTrue;
}

LispInt YacasPatternPredicateBase::LookUp(LispStringPtr aVariable)
{
    LispInt i;
    for (i=0;i<iVariables.NrItems();i++)
    {
        if (iVariables[i] == aVariable)
        {
            return i;
        }
    }
    iVariables.Append(aVariable);
    return iVariables.NrItems()-1;
}


YacasParamMatcherBase* YacasPatternPredicateBase::MakeParamMatcher(LispEnvironment& aEnvironment, LispObject* aPattern)
{
    if (aPattern == NULL)
        return NULL;

    // Deal with atoms
    if (aPattern->String())
    {
        return new MatchAtom(aPattern->String());
    }

    // Else it must be a sublist
    if (aPattern->SubList())
    {
        // See if it is a variable template:
        LispPtr* sublist = aPattern->SubList();
        LISPASSERT(sublist != NULL);

        LispInt num = InternalListLength(*sublist);

        // variable matcher here...
        if (num>1)
        {
            LispObject* head = sublist->Get();
            if (head->String() == aEnvironment.HashTable().LookUp("_"))
            {
                LispObject* second = head->Next().Get();
                if (second->String() != NULL)
                {
                    LispInt index = LookUp(second->String());

                    // Make a predicate for the type, if needed
                    if (num>2)
                    {
                        LispPtr third;
                        third.Set(second->Next().Get()->Copy(LispFalse));
                        LispCharPtr str = second->String()->String();
                        third.Get()->Next().Set(LispAtom::New(aEnvironment.HashTable().LookUp(str)));
                        LispPtr *pred = new LispPtr;
                        pred->Set(LispSubList::New(third.Get()));
                        iPredicates.Append(pred);
                    }

                    return new MatchVariable(index);
                }
            }
        }
        
        YacasParamMatcherBase** matchers = new YacasParamMatcherBase*[num];

        LispInt i;
        LispIterator iter(*sublist);
        for (i=0;i<num;i++)
        {
            matchers[i] = MakeParamMatcher(aEnvironment,iter());
            LISPASSERT(matchers[i] != NULL);
            iter.GoNext();
        }
        return new MatchSubList(matchers, num);
    }
    
    return NULL;
}

YacasPatternPredicateBase::YacasPatternPredicateBase(LispEnvironment& aEnvironment, LispPtr& aPattern,
                                                     LispPtr& aPostPredicate)
{
    LispIterator iter(aPattern);
    
    while (iter() != NULL)
    {
        YacasParamMatcherBase* matcher = MakeParamMatcher(aEnvironment,iter());
        LISPASSERT(matcher!=NULL);
        iParamMatchers.Append(matcher);
        iter.GoNext();
    }
    LispPtr* post = new LispPtr;
    post->Set(aPostPredicate.Get());
    iPredicates.Append(post);
}

LispBoolean YacasPatternPredicateBase::Matches(LispEnvironment& aEnvironment,
                                              LispPtr& aArguments)
{
    LispInt i;

    LispPtr* arguments = NULL;
    if (iVariables.NrItems() > 0)
        arguments = new LispPtr[iVariables.NrItems()];
    LocalArgs args(arguments); //Deal with destruction
    LispIterator iter(aArguments);

    for (i=0;i<iParamMatchers.NrItems();i++)
    {
        if (iter() == NULL)
            return LispFalse;
        LispPtr* ptr = iter.Ptr();
        if (ptr==NULL)
            return LispFalse;
        if (!iParamMatchers[i]->ArgumentMatches(aEnvironment,*ptr,arguments))
        {
            return LispFalse;
        }
        iter.GoNext();
    }
    if (iter() != NULL)
        return LispFalse;

    {
        // set the local variables.
        LispLocalFrame frame(aEnvironment,LispFalse);

        SetPatternVariables(aEnvironment,arguments);

        // do the predicates
        if (!CheckPredicates(aEnvironment))
            return LispFalse;
    }

    // set the local variables for sure now
    SetPatternVariables(aEnvironment,arguments);
    
    return LispTrue;
}




LispBoolean YacasPatternPredicateBase::Matches(LispEnvironment& aEnvironment,
                                              LispPtr* aArguments)
{
    LispInt i;

    LispPtr* arguments = NULL;
    if (iVariables.NrItems() > 0)
        arguments = new LispPtr[iVariables.NrItems()];
    LocalArgs args(arguments); //Deal with destruction

    for (i=0;i<iParamMatchers.NrItems();i++)
    {
        if (!iParamMatchers[i]->ArgumentMatches(aEnvironment,aArguments[i],arguments))
        {
            return LispFalse;
        }
    }

    {
        // set the local variables.
        LispLocalFrame frame(aEnvironment,LispFalse);
        SetPatternVariables(aEnvironment,arguments);

        // do the predicates
        if (!CheckPredicates(aEnvironment))
            return LispFalse;
    }

    // set the local variables for sure now
    SetPatternVariables(aEnvironment,arguments);
    return LispTrue;
}



LispBoolean YacasPatternPredicateBase::CheckPredicates(LispEnvironment& aEnvironment)
{
    LispInt i;
    for (i=0;i<iPredicates.NrItems();i++)
    {
        LispPtr pred;
        InternalEval(aEnvironment, pred, *(iPredicates[i]));
        if (IsFalse(aEnvironment, pred))
        {
            return LispFalse;
        }
    }
    return LispTrue;
}
                                                

void YacasPatternPredicateBase::SetPatternVariables(LispEnvironment& aEnvironment,
                                                    LispPtr* arguments)
{
    LispInt i;
    for (i=0;i<iVariables.NrItems();i++)
    {
        // set the variable to the new value
        aEnvironment.NewLocal(iVariables[i],arguments[i].Get());
    }
}


YacasPatternPredicateBase::~YacasPatternPredicateBase()
{
    /*TODO remove???
     LispInt i;
    for (i=0;i<iParamMatchers.NrItems();i++)
    {
        delete iParamMatchers[i];
    }
    for (i=0;i<iPredicates.NrItems();i++)
    {
        delete iPredicates[i];
        }
        */
}


