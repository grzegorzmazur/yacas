package net.sf.yacas;


/// Class for matching an expression to a given atom.
class MatchAtom extends YacasParamMatcherBase
{
  public MatchAtom(String aString)
  {
    iString = aString;
  }
  public boolean ArgumentMatches(LispEnvironment  aEnvironment,
                                      LispPtr  aExpression,
                                      LispPtr[]  arguments) throws Exception
  {
    // If it is a floating point, don't even bother comparing
    if (aExpression.Get() != null)
      if (aExpression.Get().Number(0) != null)
        if (!aExpression.Get().Number(0).IsInt())
          return false;
 
    return (iString == aExpression.Get().String());
  }
  protected String iString;
}
