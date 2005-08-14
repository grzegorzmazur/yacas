package net.sf.yacas;


class LispOperators extends LispAssociatedHash // <LispInFixOperator>
{
  public void SetOperator(int aPrecedence,String aString)
  {
    LispInFixOperator op = new LispInFixOperator(aPrecedence);
    SetAssociation(op, aString);
  }
  public void SetRightAssociative(String aString) throws Exception
  {
    LispInFixOperator op = (LispInFixOperator)LookUp(aString);
    LispError.Check(op != null,LispError.KLispErrNotAnInFixOperator);
    op.SetRightAssociative();
  }
  public void SetLeftPrecedence(String aString,int aPrecedence) throws Exception
  {
    LispInFixOperator op = (LispInFixOperator)LookUp(aString);
    LispError.Check(op != null,LispError.KLispErrNotAnInFixOperator);
    op.SetLeftPrecedence(aPrecedence);
  }
  public void SetRightPrecedence(String aString,int aPrecedence) throws Exception
  {
    LispInFixOperator op = (LispInFixOperator)LookUp(aString);
    LispError.Check(op != null,LispError.KLispErrNotAnInFixOperator);
    op.SetRightPrecedence(aPrecedence);
  }
}
