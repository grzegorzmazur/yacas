
#include "yacasprivate.h"
#include "mathuserfunc.h"
#include "lispobject.h"
#include "lispeval.h"
#include "standard.h"
#include "patterns.h"
#include "patternclass.h"
#include "substitute.h"

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

 LispBoolean BranchingUserFunction::BranchRuleTruePredicate::Matches(LispEnvironment& aEnvironment, LispPtr* aArguments)
{
    return LispTrue;
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
        arguments = NEW LispPtr[arity];
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
        BranchRuleBase* thisRule = iRules[i];
        CHECKPTR(thisRule);
        LISPASSERT(thisRule != NULL);

        st.iRulePrecedence = thisRule->Precedence();
        LispBoolean matches = thisRule->Matches(aEnvironment, arguments);
        if (matches)
        {
            st.iSide = 1;
            InternalEval(aEnvironment, aResult, thisRule->Body());
            goto FINISH;
        }

        // If rules got inserted, walk back
        while (thisRule != iRules[i] && i>0) i--;
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

LispInt BranchingUserFunction::IsArity(LispInt aArity) const
{
    return (Arity() == aArity);
}

void BranchingUserFunction::DeclareRule(LispInt aPrecedence, LispPtr& aPredicate,
                         LispPtr& aBody)
{
    // New branching rule.
    BranchRule* newRule = NEW BranchRule(aPrecedence,aPredicate,aBody);
    Check(newRule != NULL,KLispErrCreatingRule);

    InsertRule(aPrecedence,newRule);
}
 void BranchingUserFunction::DeclareRule(LispInt aPrecedence, LispPtr& aBody)
{
    // New branching rule.
    BranchRule* newRule = NEW BranchRuleTruePredicate(aPrecedence,aBody);
    Check(newRule != NULL,KLispErrCreatingRule);

    InsertRule(aPrecedence,newRule);
}

void BranchingUserFunction::DeclarePattern(LispInt aPrecedence, LispPtr& aPredicate,
                                           LispPtr& aBody)
{
    // New branching rule.
    BranchPattern* newRule = NEW BranchPattern(aPrecedence,aPredicate,aBody);
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



ListedBranchingUserFunction::ListedBranchingUserFunction(LispPtr& aParameters)
    : BranchingUserFunction(aParameters)
{
}

LispInt ListedBranchingUserFunction::IsArity(LispInt aArity) const
{
    // nr arguments handled is bound by a minimum: the number of arguments
    // to this function.
    return (Arity() <= aArity);
}

void ListedBranchingUserFunction::Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
                                           LispPtr& aArguments)
{
    LispPtr newArgs;
    LispIterator iter(aArguments);
    LispPtr* ptr =  &newArgs;
    LispInt arity = Arity();
    LispInt i=0;
    while (i < arity && iter() != NULL)
    {
        ptr->Set(iter()->Copy(LispFalse));
        ptr = &(ptr->Get()->Next());
        i++;
        iter.GoNext();
    }
    if (iter()->Next().Get() == NULL)
    {
        ptr->Set(iter()->Copy(LispFalse));
        ptr = &(ptr->Get()->Next());
        i++;
        iter.GoNext();
        LISPASSERT(iter() == NULL);
    }
    else
    {
        LispPtr head;
        head.Set(LispAtom::New(aEnvironment.iList));
        head.Get()->Next().Set(iter());
        ptr->Set(LispSubList::New(head.Get()));
    }
    BranchingUserFunction::Evaluate(aResult, aEnvironment, newArgs);
}


LispBoolean MacroUserFunction::MacroRule::Matches(LispEnvironment& aEnvironment, LispPtr* aArguments)
{
  return LispTrue;
}
LispInt MacroUserFunction::MacroRule::Precedence() const
{
  return 0;
}
LispPtr& MacroUserFunction::MacroRule::Body()
{
  return iBody;
}


MacroUserFunction::MacroUserFunction(LispPtr& aParameters)
  : BranchingUserFunction(aParameters)
{
    LispIterator iter(aParameters);
    LispInt i=0;
    while (iter())
    {
        Check(iter()->String() != NULL,KLispErrCreatingUserFunction);
        iParameters[i].iHold = LispTrue;
        iter.GoNext();
        i++;
    }
    UnFence();
}

void MacroUserFunction::Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
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
        arguments = NEW LispPtr[arity];
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

    LispPtr substedBody;
    {
      // declare a new local stack.
      LispLocalFrame frame(aEnvironment,LispTrue);
  
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
          BranchRuleBase* thisRule = iRules[i];
          CHECKPTR(thisRule);
          LISPASSERT(thisRule != NULL);
  
          st.iRulePrecedence = thisRule->Precedence();
          LispBoolean matches = thisRule->Matches(aEnvironment, arguments);
          if (matches)
          {
              st.iSide = 1;

              BackQuoteBehaviour behaviour(aEnvironment);
              InternalSubstitute(substedBody, thisRule->Body(), behaviour);
//              InternalEval(aEnvironment, aResult, thisRule->Body());
              break;
          }
  
          // If rules got inserted, walk back
          while (thisRule != iRules[i] && i>0) i--;
      }
    }
      

    if (substedBody.Get())
    {
        InternalEval(aEnvironment, aResult, substedBody);
    }
    else
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


