package net.sf.yacas;

import java.util.HashMap;

class LispOperators extends HashMap<String, LispInfixOperator>
{
  public void SetOperator(int aPrecedence,String aString)
  {
    LispInfixOperator op = new LispInfixOperator(aPrecedence);
    put(aString, op);
  }
  public void SetRightAssociative(String aString) throws Exception
  {
    LispInfixOperator op = get(aString);
    LispError.Check(op != null, LispError.KLispErrNotAnInFixOperator);
    op.SetRightAssociative();
  }
  public void SetLeftPrecedence(String aString,int aPrecedence) throws Exception
  {
    LispInfixOperator op = get(aString);
    LispError.Check(op != null, LispError.KLispErrNotAnInFixOperator);
    op.SetLeftPrecedence(aPrecedence);
  }
  public void SetRightPrecedence(String aString,int aPrecedence) throws Exception
  {
    LispInfixOperator op = get(aString);
    LispError.Check(op != null,LispError.KLispErrNotAnInFixOperator);
    op.SetRightPrecedence(aPrecedence);
  }
}
