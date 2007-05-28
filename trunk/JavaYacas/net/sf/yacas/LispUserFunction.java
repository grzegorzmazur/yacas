package net.sf.yacas;


/// Abstract class providing the basic user function API.
/// Instances of this class are associated to the name of the function
/// via an associated hash table. When obtained, they can be used to
/// evaluate the function with some arguments.

abstract class LispUserFunction extends EvalFuncBase
{
    public LispUserFunction()
    {
      iFenced = true;
      iTraced = false;
    }
    public abstract void Evaluate(LispPtr aResult,LispEnvironment aEnvironment, LispPtr aArguments) throws Exception;
    public abstract void HoldArgument(String aVariable);
    public abstract void DeclareRule(int aPrecedence, LispPtr aPredicate, LispPtr aBody) throws Exception;
    public abstract void DeclareRule(int aPrecedence, LispPtr aBody) throws Exception;
    public abstract void DeclarePattern(int aPrecedence, LispPtr aPredicate, LispPtr aBody) throws Exception;
    public abstract LispPtr ArgList();

    public void UnFence()
    {
      iFenced = false;
    }
    public boolean Fenced()
    {
      return iFenced;
    }

    public void Trace()
    {
      iTraced = true;
    }
    public void UnTrace()
    {
      iTraced = false;
    }
    public boolean Traced()
    {
      return iTraced;
    }

    boolean iFenced;
    boolean iTraced;
};
