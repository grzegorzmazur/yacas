package net.sf.yacas;


class LispInFixOperator
{
  public LispInFixOperator(int aPrecedence)
  {
    iPrecedence = aPrecedence;
    iLeftPrecedence = aPrecedence;
    iRightPrecedence = aPrecedence;
    iRightAssociative = 0;
  }
  public void SetRightAssociative()
  {
      iRightAssociative = 1;
  }
  public void SetLeftPrecedence(int aPrecedence)
  {
      iLeftPrecedence = aPrecedence;
  }
  public void SetRightPrecedence(int aPrecedence)
  {
      iRightPrecedence = aPrecedence;
  }
  public int iPrecedence;
  public int iLeftPrecedence;
  public int iRightPrecedence;
  public int iRightAssociative;
}

