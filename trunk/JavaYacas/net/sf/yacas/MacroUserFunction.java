package net.sf.yacas;


class MacroUserFunction extends BranchingUserFunction
{
  public MacroUserFunction(LispPtr  aParameters) throws Exception
  {
    super(aParameters);
    LispIterator iter = new LispIterator(aParameters);
    int i=0;
    while (iter.GetObject() != null)
    {
        LispError.Check(iter.GetObject().String() != null,LispError.KLispErrCreatingUserFunction);
        ((BranchParameter)iParameters.get(i)).iHold = true;
        iter.GoNext();
        i++;
    }
    UnFence();
  }
  public void Evaluate(LispPtr  aResult,LispEnvironment  aEnvironment,
                LispPtr  aArguments) throws Exception
  {
    int arity = Arity();
    int i;

    //hier
/*TODO fixme
    if (Traced())
    {
        LispPtr tr;
        tr.Set(LispSubList.New(aArguments.Get()));
        TraceShowEnter(aEnvironment,tr);
        tr.Set(null);
    }
*/
    LispIterator iter = new LispIterator(aArguments);
    iter.GoNext();

    // unrollable arguments
    LispPtr[] arguments;
    if (arity==0)
        arguments = null;
    else
    {
        LispError.LISPASSERT(arity>0);
        arguments = new LispPtr[arity];
    }

    // Walk over all arguments, evaluating them as necessary
    for (i=0;i<arity;i++)
    {
        arguments[i] = new LispPtr();
        LispError.Check(iter.GetObject() != null, LispError.KLispErrWrongNumberOfArgs);
        if (((BranchParameter)iParameters.get(i)).iHold)
        {
          arguments[i].Set(iter.GetObject().Copy(false));
        }
        else
        {
            LispError.Check(iter.Ptr() != null, LispError.KLispErrWrongNumberOfArgs);
            aEnvironment.iEvaluator.Eval(aEnvironment, arguments[i], iter.Ptr());
        }
        iter.GoNext();
    }
/*TODO fixme
    if (Traced())
    {
        LispIterator iter = new LispIterator(aArguments);
        iter.GoNext();
        for (i=0;i<arity;i++)
        {
            TraceShowArg(aEnvironment,*iter.Ptr(),
                  arguments[i]);

            iter.GoNext();
        }
    }
*/
    LispPtr substedBody = new LispPtr();
    {
      // declare a new local stack.
      aEnvironment.PushLocalFrame(false);
      try
      {
        // define the local variables.
        for (i=0;i<arity;i++)
        {
            String variable = ((BranchParameter)iParameters.get(i)).iParameter;
            // set the variable to the new value
            aEnvironment.NewLocal(variable,arguments[i].Get());
        }
 
        // walk the rules database, returning the evaluated result if the
        // predicate is true.
        int nrRules = iRules.size();
        UserStackInformation st = aEnvironment.iEvaluator.StackInformation();
        for (i=0;i<nrRules;i++)
        {
            BranchRuleBase thisRule = ((BranchRuleBase)iRules.get(i));
//TODO remove            CHECKPTR(thisRule);
            LispError.LISPASSERT(thisRule != null);
 
            st.iRulePrecedence = thisRule.Precedence();
            boolean matches = thisRule.Matches(aEnvironment, arguments);
            if (matches)
            {
                st.iSide = 1;

                BackQuoteBehaviour behaviour = new BackQuoteBehaviour(aEnvironment);
                LispStandard.InternalSubstitute(substedBody, thisRule.Body(), behaviour);
  //              aEnvironment.iEvaluator.Eval(aEnvironment, aResult, thisRule.Body());
                break;
            }
 
            // If rules got inserted, walk back
            while (thisRule != ((BranchRuleBase)iRules.get(i)) && i>0) i--;
        }
      }
      catch (Exception e)
      {
        throw e;
      }
      finally
      {
        aEnvironment.PopLocalFrame();
      }
    }
 

    if (substedBody.Get() != null)
    {
        aEnvironment.iEvaluator.Eval(aEnvironment, aResult, substedBody);
    }
    else
    // No predicate was true: return a new expression with the evaluated
    // arguments.
      {
        LispPtr full = new LispPtr();
        full.Set(aArguments.Get().Copy(false));
        if (arity == 0)
        {
            full.Get().Next().Set(null);
        }
        else
        {
            full.Get().Next().Set(arguments[0].Get());
            for (i=0;i<arity-1;i++)
            {
                arguments[i].Get().Next().Set(arguments[i+1].Get());
            }
        }
        aResult.Set(LispSubList.New(full.Get()));
    }
//FINISH:
/*TODO fixme
    if (Traced())
    {
        LispPtr tr;
        tr.Set(LispSubList.New(aArguments.Get()));
        TraceShowLeave(aEnvironment, aResult,tr);
        tr.Set(null);
    }
*/
  }
}


