package net.sf.yacas;


/// Class for matching against a pattern variable.
class MatchVariable extends YacasParamMatcherBase
{
  public MatchVariable(int aVarIndex)
  {
    iVarIndex = aVarIndex;
  }

  /// Matches an expression against the pattern variable.
  /// \param aEnvironment the underlying Lisp environment.
  /// \param aExpression the expression to test.
  /// \param arguments (input/output) actual values of the pattern
  /// variables for \a aExpression.
  ///
  /// If entry #iVarIndex in \a arguments is still empty, the
  /// pattern matches and \a aExpression is stored in this
  /// entry. Otherwise, the pattern only matches if the entry equals
  /// \a aExpression.
  public boolean ArgumentMatches(LispEnvironment  aEnvironment,
                                      LispPtr  aExpression,
                                      LispPtr[]  arguments) throws Exception
  {
// this should not be necessary
//    if (arguments[iVarIndex] == null)
//    {
//      arguments[iVarIndex] = new LispPtr();
//    }
    if (arguments[iVarIndex].Get() == null)
    {
        arguments[iVarIndex].Set(aExpression.Get());
//        LogPrintf("Set var %d\n",iVarIndex);
        return true;
    }
    else
    {
        if (LispStandard.InternalEquals(aEnvironment, aExpression, arguments[iVarIndex]))
        {
//            LogPrintf("Matched var %d\n",iVarIndex);
            return true;
        }
        return false;
    }
//    return false;
  }

  /// Index of variable in YacasPatternPredicateBase.iVariables.
  protected int iVarIndex;

  /// Not used.
  protected String iString;
};
