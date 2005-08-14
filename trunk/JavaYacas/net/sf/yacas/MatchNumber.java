package net.sf.yacas;


/// Class for matching an expression to a given number.
class MatchNumber extends YacasParamMatcherBase
{
  public MatchNumber(BigNumber aNumber)
  {
    iNumber = aNumber;
  }
  public boolean ArgumentMatches(LispEnvironment  aEnvironment,
                                      LispPtr  aExpression,
                                      LispPtr[]  arguments) throws Exception
  {
    if (aExpression.Get().Number(aEnvironment.Precision()) != null)
      return iNumber.Equals(aExpression.Get().Number(aEnvironment.Precision()));
    return false;
  }
  protected BigNumber iNumber;
}
