
#include "mathuserfunc.h"
#include "lispobject.h"
#include "lispeval.h"
#include "standard.h"
#include "patterns.h"
#include "patternclass.h"

#define InternalEval aEnvironment.iEvaluator->Eval

BranchingUserFunction::BranchRuleBase::~BranchRuleBase()
{
}

LispBoolean BranchingUserFunction::BranchRule::Matches(LispEnvironment& aEnvironment, LispPtr* aArguments)
{
    LispPtr pred;
    InternalEval(aEnvironment, pred, iPredicate);
    return IsTrue(aEnvironment,pred);
}
LispInt BranchingUserFunction::BranchRule::Precedence() const
{
    return iPrecedence;
}
LispPtr& BranchingUserFunction::BranchRule::Body()
{
    return iBody;
}
BranchingUserFunction::BranchRule::~BranchRule()
{
}

LispBoolean BranchingUserFunction::BranchPattern::Matches(LispEnvironment& aEnvironment, LispPtr* aArguments)
{
    return iPatternClass->Matches(aEnvironment,aArguments);
}
LispInt BranchingUserFunction::BranchPattern::Precedence() const
{
    return iPrecedence;
}
LispPtr& BranchingUserFunction::BranchPattern::Body()
{
    return iBody;
}
BranchingUserFunction::BranchPattern::~BranchPattern()
{
}


BranchingUserFunction::BranchingUserFunction(LispPtr& aParameters)
    
{
    iParamList.Set(aParameters.Get());

    LispIterator iter(aParameters);

    while (iter())
    {
        Check(iter()->String() != NULL,KLispErrCreatingUserFunction);
        BranchParameter param(iter()->String());
        iParameters.Append(param);
        iter.GoNext();
    }
}

BranchingUserFunction::~BranchingUserFunction()
{
    /*TODO remove???
     LispInt i;
    LispInt nrc=iRules.NrItems();
    for (i=0;i<nrc;i++)
    {
        delete iRules[i];
        iRules[i] = NULL;
        }
        */
}

void BranchingUserFunction::Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
                                     LispPtr& aArguments)
{
    LispInt arity = Arity();
    LispInt i;

    //hier
    if (Traced())
    {
        LispPtr tr;
        tr.Set(LispSubList::New(aArguments.Get()));
        TraceShowEnter(aEnvironment,tr);
        tr.Set(NULL);
    }
    
    LispIterator iter(aArguments);
    iter.GoNext();

    // unrollable arguments 
    LispPtr* arguments;
    if (arity==0)
        arguments = NULL;
    else
    {
        LISPASSERT(arity>0);
        arguments = new LispPtr[arity];
    }
    LocalArgs args(arguments);

    // Walk over all arguments, evaluating them as necessary
    for (i=0;i<arity;i++)
    {
        Check(iter() != NULL, KLispErrWrongNumberOfArgs);
        if (iParameters[i].iHold)
        {
            arguments[i].Set(iter()->Copy(LispFalse));
        }
        else
        {
            Check(iter.Ptr() != NULL, KLispErrWrongNumberOfArgs);
            InternalEval(aEnvironment, arguments[i], *iter.Ptr());
        }
        iter.GoNext();
    }

    if (Traced())
    {
        LispIterator iter(aArguments);
        iter.GoNext();
        for (i=0;i<arity;i++)
        {
            TraceShowArg(aEnvironment,*iter.Ptr(),
                  arguments[i]);

            iter.GoNext();
        }
    }
    
    // declare a new local stack.
    LispLocalFrame frame(aEnvironment,Fenced());

    // define the local variables.
    for (i=0;i<arity;i++)
    {
        LispStringPtr variable = iParameters[i].iParameter;
        // set the variable to the new value
        aEnvironment.NewLocal(variable,arguments[i].Get());
    }

    // walk the rules database, returning the evaluated result if the
    // predicate is LispTrue.
    LispInt nrRules = iRules.NrItems();
    UserStackInformation &st = aEnvironment.iEvaluator->StackInformation();
    for (i=0;i<nrRules;i++)
    {
        LISPASSERT(iRules[i] != NULL);
        st.iRulePrecedence = iRules[i]->Precedence();
        LispBoolean matches = iRules[i]->Matches(aEnvironment, arguments);
        if (matches)
        {
            st.iSide = 1;
            InternalEval(aEnvironment, aResult, iRules[i]->Body());
            goto FINISH;
        }
    }
    
    // No predicate was LispTrue: return a new expression with the evaluated
    // arguments.

    {
        LispPtr full;
        full.Set(aArguments.Get()->Copy(LispFalse));
        if (arity == 0)
        {
            full.Get()->Next().Set(NULL);
        }
        else
        {
            full.Get()->Next().Set(arguments[0].Get());
            for (i=0;i<arity-1;i++)
            {
                arguments[i].Get()->Next().Set(arguments[i+1].Get());
            }
        }
        aResult.Set(LispSubList::New(full.Get()));
    }

FINISH:
    if (Traced())
    {
        LispPtr tr;
        tr.Set(LispSubList::New(aArguments.Get()));
        TraceShowLeave(aEnvironment, aResult,tr);
        tr.Set(NULL);
    }
}

void BranchingUserFunction::HoldArgument(LispStringPtr aVariable)
{
    LispInt i;
    LispInt nrc=iParameters.NrItems();
    for (i=0;i<nrc;i++)
    {
        if (iParameters[i].iParameter == aVariable)
            iParameters[i].iHold = LispTrue;
    }
}

LispInt BranchingUserFunction::Arity() const
{
    return iParameters.NrItems();
}

void BranchingUserFunction::DeclareRule(LispInt aPrecedence, LispPtr& aPredicate,
                         LispPtr& aBody)
{
    // New branching rule.
    BranchRule* newRule = new BranchRule(aPrecedence,aPredicate,aBody);
    Check(newRule != NULL,KLispErrCreatingRule);

    InsertRule(aPrecedence,newRule);
}

void BranchingUserFunction::DeclarePattern(LispInt aPrecedence, LispPtr& aPredicate,
                                           LispPtr& aBody)
{
    // New branching rule.
    BranchPattern* newRule = new BranchPattern(aPrecedence,aPredicate,aBody);
    Check(newRule != NULL,KLispErrCreatingRule);

    InsertRule(aPrecedence,newRule);
}


void BranchingUserFunction::InsertRule(LispInt aPrecedence,BranchRuleBase* newRule)
{
    // Find place to insert
    LispInt low,high,mid;
    low=0;
    high=iRules.NrItems();

    // Constant time: find out if the precedence is before any of the
    // currently defined rules or past them.
    if (high>0)
    {
        if (iRules[0]->Precedence() > aPrecedence)
        {
            mid=0;
            goto CONTINUE;
        }
        if (iRules[high-1]->Precedence() < aPrecedence)
        {
            mid=high;
            goto CONTINUE;
        }
    }

    // Otherwise, O(log n) search algorithm for place to insert
    for(;;)
    {
        if (low>=high)
        {
            mid=low;
            goto CONTINUE;
        }
        mid = (low+high)>>1;

        if (iRules[mid]->Precedence() > aPrecedence)
        {
            high = mid;
        }
        else if (iRules[mid]->Precedence() < aPrecedence)
        {
            low = (++mid);
        }
        else
        {
            goto CONTINUE;
        }
    }
    CONTINUE:
    // Insert it
    iRules.Insert(mid,newRule);
}


LispPtr& BranchingUserFunction::ArgList()
{
    return iParamList;
}





