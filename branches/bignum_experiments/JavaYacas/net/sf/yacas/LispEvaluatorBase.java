package net.sf.yacas;


/// Abstract evaluator for Lisp expressions.
/// Eval() is a pure virtual function, to be provided by the derived class.
/// The other functions are stubs.

abstract class LispEvaluatorBase
{
    public abstract void Eval(LispEnvironment aEnvironment, LispPtr aResult, LispPtr aExpression) throws Exception;
    public void ResetStack()
    {
    }
    public UserStackInformation StackInformation()
    {
      return iBasicInfo;
    }
    public void ShowStack(LispEnvironment aEnvironment, LispOutput aOutput)
    {
    }
    UserStackInformation iBasicInfo = new UserStackInformation();
};
