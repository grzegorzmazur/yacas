package net.sf.yacas;


import java.util.*;

/// A mathematical function defined by several rules.
/// This is the basic class which implements functions in Yacas.
/// Evaluation is done by consulting a set of rewriting rules. The
/// body of the first rule that matches, is evaluated and this gives
/// the result of evaluating the function.

class BranchingUserFunction extends LispArityUserFunction
{
  /// Structure containing name of parameter and whether it is put on hold.
  class BranchParameter
  {
    public BranchParameter(String aParameter, boolean aHold /*=false*/)
    {
      iParameter = aParameter;
      iHold = aHold;
    }
    String  iParameter;
    boolean iHold;
  }

  /// Abstract base class for rules.
  abstract class BranchRuleBase
  {
    public abstract boolean Matches(LispEnvironment aEnvironment, LispPtr[] aArguments) throws Exception;
    public abstract int Precedence();
    public abstract LispPtr Body();
  }

  /// A rule with a predicate.
  /// This rule matches if the predicate evaluates to #true.
  class BranchRule extends BranchRuleBase
  {
    public BranchRule(int aPrecedence,LispPtr  aPredicate,LispPtr  aBody)
    {
      iPrecedence = aPrecedence;
      iPredicate.Set(aPredicate.Get());
      iBody.Set(aBody.Get());
    }

    /// Return true if the rule matches.
    /// #iPredicate is evaluated in \a Environment. If the result
    /// IsTrue(), this function returns true.
    public boolean Matches(LispEnvironment  aEnvironment, LispPtr[] aArguments) throws Exception
    {
      LispPtr pred = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, pred, iPredicate);
      return LispStandard.IsTrue(aEnvironment,pred);
    }

    /// Access #iPrecedence.
    public int Precedence()
    {
      return iPrecedence;
    }

    /// Access #iBody.
    public LispPtr Body()
    {
      return iBody;
    }
    protected BranchRule()
    {
    }
    protected int iPrecedence;
    protected LispPtr iBody = new LispPtr();
    protected LispPtr iPredicate = new LispPtr();
  }

  /// A rule that always matches.
  class BranchRuleTruePredicate extends BranchRule
  {
    public BranchRuleTruePredicate(int aPrecedence,LispPtr  aBody)
    {
      iPrecedence = aPrecedence;
      iBody.Set(aBody.Get());
    }
    /// Return #true, always.
    public boolean Matches(LispEnvironment  aEnvironment, LispPtr[] aArguments) throws Exception
    {
      return true;
    }
  }

  /// A rule which matches if the corresponding PatternClass matches.
  class BranchPattern extends BranchRuleBase
  {
    /// Constructor.
    /// \param aPrecedence precedence of the rule
    /// \param aPredicate generic object of type \c Pattern
    /// \param aBody body of the rule
    public BranchPattern(int aPrecedence,LispPtr  aPredicate,LispPtr  aBody) throws Exception
    {
      iPatternClass = null;
      iPrecedence = aPrecedence;
      iPredicate.Set(aPredicate.Get());

      GenericClass gen = aPredicate.Get().Generic();
      LispError.Check(gen != null,LispError.KLispErrInvalidArg);
      LispError.Check(gen.TypeName().equals("\"Pattern\""),LispError.KLispErrInvalidArg);

      iPatternClass = (PatternClass)gen;
      iBody.Set(aBody.Get());
    }

    /// Return true if the corresponding pattern matches.
    public boolean Matches(LispEnvironment  aEnvironment, LispPtr[] aArguments) throws Exception
    {
      return iPatternClass.Matches(aEnvironment,aArguments);
    }

    /// Access #iPrecedence
    public int Precedence()
    {
      return iPrecedence;
    }

    /// Access #iBody
    public LispPtr Body()
    {
      return iBody;
    }

    /// The precedence of this rule.
    protected int iPrecedence;

    /// The body of this rule.
    protected LispPtr iBody = new LispPtr();

    /// Generic object of type \c Pattern containing #iPatternClass
    protected LispPtr iPredicate = new LispPtr();

    /// The pattern that decides whether this rule matches.
    protected PatternClass iPatternClass;
  };

  /// Constructor.
  /// \param aParameters linked list constaining the names of the arguments
  ///
  /// #iParamList and #iParameters are set from \a aParameters.
  BranchingUserFunction(LispPtr aParameters) throws Exception
  {
    iParamList.Set(aParameters.Get());
    LispIterator iter = new LispIterator(aParameters);
    while (iter.GetObject() != null)
    {
      LispError.Check(iter.GetObject().String() != null,LispError.KLispErrCreatingUserFunction);
      BranchParameter param = new BranchParameter(iter.GetObject().String(),false);
      iParameters.add(param);
      iter.GoNext();
    }
  }

  /// Evaluate the function on given arguments.
  /// \param aResult (on output) the result of the evaluation
  /// \param aEnvironment the underlying Lisp environment
  /// \param aArguments the arguments to the function
  ///
  /// First, all arguments are evaluated by the evaluator associated
  /// to \a aEnvironment, unless the \c iHold flag of the
  /// corresponding parameter is true. Then a new LispLocalFrame is
  /// constructed, in which the actual arguments are assigned to the
  /// names of the formal arguments, as stored in \c iParameter. Then
  /// all rules in #iRules are tried one by one. The body of the
  /// first rule that matches is evaluated, and the result is put in
  /// \a aResult. If no rule matches, \a aResult will recieve a new
  /// expression with evaluated arguments.
  public void Evaluate(LispPtr aResult,LispEnvironment aEnvironment, LispPtr aArguments) throws Exception
  {
    int arity = Arity();
    int i;

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
        for (i=0;i<arity;i++)
          arguments[i] = new LispPtr();
    }

    // Walk over all arguments, evaluating them as necessary
    for (i=0;i<arity;i++)
    {
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
    // declare a new local stack.
    aEnvironment.PushLocalFrame(Fenced());
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
          LispError.LISPASSERT(thisRule != null);

          st.iRulePrecedence = thisRule.Precedence();
          boolean matches = thisRule.Matches(aEnvironment, arguments);
          if (matches)
          {
              st.iSide = 1;
              aEnvironment.iEvaluator.Eval(aEnvironment, aResult, thisRule.Body());
  /*TODO fixme
              if (Traced())
              {
                  LispPtr tr;
                  tr.Set(LispSubList.New(aArguments.Get()));
                  TraceShowLeave(aEnvironment, aResult,tr);
                  tr.Set(null);
              }
  */
              return;
          }

          // If rules got inserted, walk back
          while (thisRule != ((BranchRuleBase)iRules.get(i)) && i>0) i--;
      }
 
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
    catch (Exception e)
    {
      throw e;
    }
    finally
    {
      aEnvironment.PopLocalFrame();
    }
  }

  /// Put an argument on hold.
  /// \param aVariable name of argument to put un hold
  ///
  /// The \c iHold flag of the corresponding argument is set. This
  /// implies that this argument is not evaluated by Evaluate().
  public void HoldArgument(String aVariable)
  {
    int i;
    int nrc=iParameters.size();
    for (i=0;i<nrc;i++)
    {
        if (((BranchParameter)iParameters.get(i)).iParameter == aVariable)
            ((BranchParameter)iParameters.get(i)).iHold = true;
    }
  }

  /// Return true if the arity of the function equals \a aArity.
  public boolean IsArity(int aArity)
  {
    return (Arity() == aArity);
  }

  /// Return the arity (number of arguments) of the function.
  public int Arity()
  {
    return iParameters.size();
  }

  /// Add a BranchRule to the list of rules.
  /// \sa InsertRule()
  public void DeclareRule(int aPrecedence, LispPtr aPredicate, LispPtr aBody) throws Exception
  {
    // New branching rule.
    BranchRule newRule = new BranchRule(aPrecedence,aPredicate,aBody);
    LispError.Check(newRule != null,LispError.KLispErrCreatingRule);

    InsertRule(aPrecedence,newRule);
  }

  /// Add a BranchRuleTruePredicate to the list of rules.
  /// \sa InsertRule()
  public void DeclareRule(int aPrecedence, LispPtr aBody) throws Exception
  {
    // New branching rule.
    BranchRule newRule = new BranchRuleTruePredicate(aPrecedence,aBody);
    LispError.Check(newRule != null,LispError.KLispErrCreatingRule);

    InsertRule(aPrecedence,newRule);
  }

  /// Add a BranchPattern to the list of rules.
  /// \sa InsertRule()
  public void DeclarePattern(int aPrecedence, LispPtr aPredicate, LispPtr aBody) throws Exception
  {
    // New branching rule.
    BranchPattern newRule = new BranchPattern(aPrecedence,aPredicate,aBody);
    LispError.Check(newRule != null,LispError.KLispErrCreatingRule);

    InsertRule(aPrecedence,newRule);
  }

  /// Insert any BranchRuleBase object in the list of rules.
  /// This function does the real work for DeclareRule() and
  /// DeclarePattern(): it inserts the rule in #iRules, while
  /// keeping it sorted. The algorithm is \f$O(\log n)\f$, where
  /// \f$n\f$ denotes the number of rules.
  void InsertRule(int aPrecedence,BranchRuleBase newRule)
  {
    // Find place to insert
    int low,high,mid;
    low=0;
    high=iRules.size();

    // Constant time: find out if the precedence is before any of the
    // currently defined rules or past them.
    if (high>0)
    {
        if (((BranchRuleBase)iRules.get(0)).Precedence() > aPrecedence)
        {
            mid=0;
            // Insert it
            iRules.add(mid,newRule);return;
        }
        if (((BranchRuleBase)iRules.get(high-1)).Precedence() < aPrecedence)
        {
            mid=high;
            // Insert it
            iRules.add(mid,newRule);return;
        }
    }

    // Otherwise, O(log n) search algorithm for place to insert
    for(;;)
    {
      if (low>=high)
      {
        mid=low;
        // Insert it
        iRules.add(mid,newRule);return;
      }
      mid = (low+high)>>1;

      if (((BranchRuleBase)iRules.get(mid)).Precedence() > aPrecedence)
      {
        high = mid;
      }
      else if (((BranchRuleBase)iRules.get(mid)).Precedence() < aPrecedence)
      {
        low = (++mid);
      }
      else
      {
        // Insert it
        iRules.add(mid,newRule);return;
      }
    }
  }

  /// Return the argument list, stored in #iParamList
  public LispPtr ArgList()
  {
    return iParamList;
  }

  /// List of arguments, with corresponding \c iHold property.
  protected Vector iParameters = new Vector(); //CArrayGrower<BranchParameter>

  /// List of rules, sorted on precedence.
  protected Vector iRules = new Vector();//CDeletingArrayGrower<BranchRuleBase*>

  /// List of arguments
  LispPtr iParamList = new LispPtr();
}

