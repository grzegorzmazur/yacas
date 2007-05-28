
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
  : iParameters(),iRules(),iParamList(aParameters)
{
  LispIterator iter(aParameters);
  for ( ; iter.getObj(); ++iter)
  {
    Check(iter.getObj()->String(),KLispErrCreatingUserFunction);
    BranchParameter param(iter.getObj()->String());
    iParameters.Append(param);
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
        LispPtr tr(LispSubList::New(aArguments));
        TraceShowEnter(aEnvironment,tr);
        tr = (NULL);
    }

    LispIterator iter(aArguments);
    ++iter;

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
  for (i=0;i<arity;i++,++iter)
  {
        Check(iter.getObj(), KLispErrWrongNumberOfArgs);
        if (iParameters[i].iHold)
        {
            arguments[i] = (iter.getObj()->Copy());
        }
        else
        {
            //Check(iter.getObj(), KLispErrWrongNumberOfArgs);  // checked above
            InternalEval(aEnvironment, arguments[i], *iter);
        }
  }

    if (Traced())
    {
        LispIterator iter(aArguments);
        for (i=0;i<arity;i++)
        {
            TraceShowArg(aEnvironment,*++iter,arguments[i]);
        }
  }
 
    // declare a new local stack.
    LispLocalFrame frame(aEnvironment,Fenced());

    // define the local variables.
    for (i=0;i<arity;i++)
    {
        LispString * variable = iParameters[i].iParameter;
        // set the variable to the new value
        aEnvironment.NewLocal(variable,arguments[i]);
    }

    // walk the rules database, returning the evaluated result if the
    // predicate is LispTrue.
    LispInt nrRules = iRules.Size();
    UserStackInformation &st = aEnvironment.iEvaluator->StackInformation();
    for (i=0;i<nrRules;i++)
    {
        BranchRuleBase* thisRule = iRules[i];
        CHECKPTR(thisRule);
        LISPASSERT(thisRule);

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
        LispPtr full(aArguments->Copy());
        if (arity == 0)
        {
            full->Nixed() = (NULL);
        }
        else
        {
            full->Nixed() = (arguments[0]);
            for (i=0;i<arity-1;i++)
            {
                arguments[i]->Nixed() = (arguments[i+1]);
            }
        }
        aResult = (LispSubList::New(full));
    }

FINISH:
    if (Traced())
    {
        LispPtr tr(LispSubList::New(aArguments));
        TraceShowLeave(aEnvironment, aResult,tr);
        tr = (NULL);
    }
}

void BranchingUserFunction::HoldArgument(LispString * aVariable)
{
    LispInt i;
    LispInt nrc=iParameters.Size();
    for (i=0;i<nrc;i++)
    {
        if (iParameters[i].iParameter == aVariable)
            iParameters[i].iHold = LispTrue;
    }
}

LispInt BranchingUserFunction::Arity() const
{
    return iParameters.Size();
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
    Check(newRule,KLispErrCreatingRule);

    InsertRule(aPrecedence,newRule);
}

void BranchingUserFunction::DeclareRule(LispInt aPrecedence, LispPtr& aBody)
{
    // New branching rule.
    BranchRule* newRule = NEW BranchRuleTruePredicate(aPrecedence,aBody);
    Check(newRule,KLispErrCreatingRule);

    InsertRule(aPrecedence,newRule);
}

void BranchingUserFunction::DeclarePattern(LispInt aPrecedence, LispPtr& aPredicate,
                                           LispPtr& aBody)
{
    // New branching rule.
    BranchPattern* newRule = NEW BranchPattern(aPrecedence,aPredicate,aBody);
    Check(newRule,KLispErrCreatingRule);

    InsertRule(aPrecedence,newRule);
}

void BranchingUserFunction::InsertRule(LispInt aPrecedence,BranchRuleBase* newRule)
{
    // Find place to insert
    LispInt low,high,mid;
    low=0;
    high=iRules.Size();

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
  // Make a copy of the arguments first
  // TODO: if we were to change the internal representation to a cons cell, this copying would not be needed
  LispInt i;
  for (i = 0; i < arity && iter.getObj(); i++,++iter)
  {
    (*ptr) = (iter.getObj()->Copy());
    ptr = &((*ptr)->Nixed());
  }
  if (!iter.getObj()->Nixed())
  {
    (*ptr) = (iter.getObj()->Copy());
    ptr = &((*ptr)->Nixed());
    ++iter;
    LISPASSERT(!iter.getObj());
  }
  else
  {
    LispPtr head(aEnvironment.iList->Copy());
    head->Nixed() = (iter.getObj());
    (*ptr) = (LispSubList::New(head));
  }
  BranchingUserFunction::Evaluate(aResult, aEnvironment, newArgs);
}

MacroUserFunction::MacroUserFunction(LispPtr& aParameters)
  : BranchingUserFunction(aParameters)
{
    LispIterator iter(aParameters);
    for (LispInt i=0; iter.getObj(); i++,++iter)
    {
        Check(iter.getObj()->String(),KLispErrCreatingUserFunction);
        iParameters[i].iHold = LispTrue;
    }
  UnFence();
}

void MacroUserFunction::Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
              LispPtr& aArguments)
{
  LispInt arity = Arity();
  LispInt i;

  if (Traced())
  {
    LispPtr tr(LispSubList::New(aArguments));
    TraceShowEnter(aEnvironment,tr);
    tr = (NULL);
  }

  LispIterator iter(aArguments);
  ++iter;

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
  for (i=0;i<arity;i++,++iter)
  {
    Check(iter.getObj(), KLispErrWrongNumberOfArgs);
    if (iParameters[i].iHold)
    {
      arguments[i] = (iter.getObj()->Copy());
    }
    else
    {
      InternalEval(aEnvironment, arguments[i], *iter);
    }
  }

  if (Traced())
  {
    LispIterator iter(aArguments);
    // TODO: ideally we would only need an iterator here
    ++iter;
    for (i=0;i<arity;i++)
    {
      TraceShowArg(aEnvironment,*iter,arguments[i]);
      ++iter;
    }
  }

  LispPtr substedBody;
  {
    // declare a new local stack.
    LispLocalFrame frame(aEnvironment,LispFalse);

    // define the local variables.
    for (i=0;i<arity;i++)
    {
      LispString * variable = iParameters[i].iParameter;
      // set the variable to the new value
      aEnvironment.NewLocal(variable,arguments[i]);
    }

    // walk the rules database, returning the evaluated result if the
    // predicate is LispTrue.
    LispInt nrRules = iRules.Size();
    UserStackInformation &st = aEnvironment.iEvaluator->StackInformation();
    for (i=0;i<nrRules;i++)
    {
      BranchRuleBase* thisRule = iRules[i];
      CHECKPTR(thisRule);
      LISPASSERT(thisRule);

      st.iRulePrecedence = thisRule->Precedence();
      LispBoolean matches = thisRule->Matches(aEnvironment, arguments);
      if (matches)
      {
        st.iSide = 1;

        BackQuoteBehaviour behaviour(aEnvironment);
        InternalSubstitute(substedBody, thisRule->Body(), behaviour);
        break;
      }
 
      // If rules got inserted, walk back
      while (thisRule != iRules[i] && i>0) i--;
    }
  }

  if (!!substedBody)
  {
    InternalEval(aEnvironment, aResult, substedBody);
  }
  else
  // No predicate was LispTrue: return a new expression with the evaluated
  // arguments.
  {
    LispPtr full(aArguments->Copy());
    if (arity == 0)
    {
      full->Nixed() = (NULL);
    }
    else
    {
      full->Nixed() = (arguments[0]);
      for (i=0;i<arity-1;i++)
      {
        arguments[i]->Nixed() = (arguments[i+1]);
      }
    }
    aResult = (LispSubList::New(full));
  }
  if (Traced())
  {
    LispPtr tr(LispSubList::New(aArguments));
    TraceShowLeave(aEnvironment, aResult,tr);
    tr = (NULL);
  }
}

ListedMacroUserFunction::ListedMacroUserFunction(LispPtr& aParameters)
  : MacroUserFunction(aParameters)
{
}

LispInt ListedMacroUserFunction::IsArity(LispInt aArity) const
{
    return (Arity() <= aArity);
}

void ListedMacroUserFunction::Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
              LispPtr& aArguments)
{
  LispPtr newArgs;
  LispIterator iter(aArguments);
  LispPtr* ptr =  &newArgs;
  LispInt arity = Arity();
  LispInt i=0;
  // TODO: the code would look a lot easier if we could do with only an iterator
  while (i < arity && iter.getObj())
  {
    (*ptr) = (iter.getObj()->Copy());
    ptr = &((*ptr)->Nixed());
    i++;
    ++iter;
  }
  if (!iter.getObj()->Nixed())
  {
    (*ptr) = (iter.getObj()->Copy());
    ptr = &((*ptr)->Nixed());
    i++;
    ++iter;
    LISPASSERT(!iter.getObj());
  }
  else
  {
    LispPtr head(aEnvironment.iList->Copy());
    head->Nixed() = (iter.getObj());
    (*ptr) = (LispSubList::New(head));
  }
  MacroUserFunction::Evaluate(aResult, aEnvironment, newArgs);
}
