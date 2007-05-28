package net.sf.yacas;

/** subst behaviour for backquote mechanism as in LISP.
 * When typing `(...) all occurrences of @a will be
 * replaced with:
 * 1) a evaluated if a is an atom
 * 2) function call with function name replaced by evaluated
 *    head of function if a is a function. For instance, if
 *    a is f(x) and f is g, then f(x) gets replaced by g(x)
 */
class BackQuoteBehaviour implements SubstBehaviourBase
{

    public BackQuoteBehaviour(LispEnvironment aEnvironment)
    {
      iEnvironment = aEnvironment;
    }
    public boolean Matches(LispPtr aResult, LispPtr aElement) throws Exception
    {
      if (aElement.Get().SubList() == null) return false;
      LispObject ptr = aElement.Get().SubList().Get();
      if (ptr == null) return false;
      if (ptr.String() == null) return false;

      if (ptr.String().equals("`"))
      {
        aResult.Set(aElement.Get());
        return true;
      }

      if (!ptr.String().equals("@"))
        return false;
      ptr = ptr.Next().Get();
      if (ptr == null)
        return false;
      if (ptr.String() != null)
      {
        LispPtr cur = new LispPtr();
        cur.Set(ptr);
        iEnvironment.iEvaluator.Eval(iEnvironment, aResult, cur);
        return true;
      }
      else
      {
        ptr = ptr.SubList().Get();
        LispPtr cur = new LispPtr();
        cur.Set(ptr);
        LispPtr args = new LispPtr();
        args.Set(ptr.Next().Get());
        LispPtr result = new LispPtr();
        iEnvironment.iEvaluator.Eval(iEnvironment, result, cur);
        result.Get().Next().Set(args.Get());
        LispPtr result2 = new LispPtr();
        result2.Set(LispSubList.New(result.Get()));
        LispStandard.InternalSubstitute(aResult, result2,this);
        return true;
      }
//      return false;
    }
    LispEnvironment iEnvironment;
};
