package net.sf.yacas;


/** Behaviour for substituting sub-expressions.
 */
interface SubstBehaviourBase
{
  public boolean Matches(LispPtr aResult, LispPtr aElement) throws Exception;
};
