package net.sf.yacas;


/// Class for matching against a list of YacasParamMatcherBase objects.
class MatchSubList extends YacasParamMatcherBase
{
  public MatchSubList(YacasParamMatcherBase[] aMatchers, int aNrMatchers)
  {
    iMatchers = aMatchers;
    iNrMatchers = aNrMatchers;
  }

  public boolean ArgumentMatches(LispEnvironment  aEnvironment,
                                      LispPtr  aExpression,
                                      LispPtr[]  arguments) throws Exception
  {
    if (aExpression.Get().SubList() == null)
        return false;
    int i;

    LispIterator iter = new LispIterator(aExpression);
    iter.GoSub();
 
    for (i=0;i<iNrMatchers;i++)
    {
        LispPtr  ptr = iter.Ptr();
        if (ptr == null)
            return false;
        if (iter.GetObject() == null)
            return false;
        if (!iMatchers[i].ArgumentMatches(aEnvironment,ptr,arguments))
            return false;
        iter.GoNext();
    }
    if (iter.GetObject() != null)
        return false;
    return true;
  }

  protected YacasParamMatcherBase[] iMatchers;
  protected int iNrMatchers;
}
