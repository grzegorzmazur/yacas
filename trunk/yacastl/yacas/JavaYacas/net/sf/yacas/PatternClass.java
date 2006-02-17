package net.sf.yacas;


/// Wrapper for YacasPatternPredicateBase.
/// This class allows a YacasPatternPredicateBase to be put in a
/// LispGenericObject.
class PatternClass extends GenericClass
{
  public PatternClass(YacasPatternPredicateBase aPatternMatcher)
  {
    iPatternMatcher = aPatternMatcher;
  }

  public boolean Matches(LispEnvironment  aEnvironment, LispPtr aArguments) throws Exception
  {
    LispError.LISPASSERT(iPatternMatcher != null);
    boolean result;
    result = iPatternMatcher.Matches(aEnvironment, aArguments);
    return result;
  }
  public boolean Matches(LispEnvironment  aEnvironment, LispPtr[] aArguments) throws Exception
  {
    LispError.LISPASSERT(iPatternMatcher != null);
    boolean result;
    result = iPatternMatcher.Matches(aEnvironment, aArguments);
    return result;
  }
  //From GenericClass
  public String Send(LispArgList aArgList)
  {
    return null;
  }
  public String TypeName()
  {
      return "\"Pattern\"";
  }

  protected YacasPatternPredicateBase iPatternMatcher;
}


