package net.sf.yacas;


/// Value of a Lisp global variable.
/// The only special feature of this class is the attribute
/// #iEvalBeforeReturn, which defaults to #LispFalse. If this
/// attribute is set to #LispTrue, the value in #iValue needs to be
/// evaluated to get the value of the Lisp variable.
/// \sa LispEnvironment::GetVariable()

class LispGlobalVariable
{
  public LispGlobalVariable(LispGlobalVariable aOther)
  {
    iValue = aOther.iValue;
    iEvalBeforeReturn = aOther.iEvalBeforeReturn;
  }
  public LispGlobalVariable(LispPtr aValue)
  {
    iValue.Set(aValue.Get());
    iEvalBeforeReturn = false;
  }
  public  void SetEvalBeforeReturn(boolean aEval)
  {
   iEvalBeforeReturn = aEval;
  }
  LispPtr iValue = new LispPtr();
  boolean iEvalBeforeReturn;
}
