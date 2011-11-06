package net.sf.yacas;


/** Substing one expression for another. The simplest form
 * of substitution
 */
class SubstBehaviour implements SubstBehaviourBase
{
    public SubstBehaviour(LispEnvironment aEnvironment,LispPtr aToMatch, LispPtr aToReplaceWith)
    {
      iEnvironment = aEnvironment;
      iToMatch = aToMatch;
      iToReplaceWith = aToReplaceWith;
    }
    public boolean Matches(LispPtr aResult, LispPtr aElement) throws Exception
    {
      if (LispStandard.InternalEquals(iEnvironment, aElement, iToMatch))
      {
          aResult.Set(iToReplaceWith.Get().Copy(false));
          return true;
      }
      return false;
    }
    LispEnvironment iEnvironment;
    LispPtr iToMatch;
    LispPtr iToReplaceWith;
};
