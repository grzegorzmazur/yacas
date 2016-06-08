package net.sf.yacas;


class StringInput extends LispInput
{
  public StringInput(StringBuffer aString, InputStatus aStatus)
  {
    super(aStatus);
    iString = aString;
    iCurrent = 0;
  }
  @Override
  public char Next() throws Exception
  {
    if (iCurrent == iString.length())
      return '\0';
    iCurrent++;
    char c = iString.charAt(iCurrent-1);
    if (c == '\n')
      iStatus.NextLine();
    return c;
  }
  @Override
  public char Peek() throws Exception
  {
    if (iCurrent == iString.length())
      return '\0';
    return iString.charAt(iCurrent);
  }
  @Override
  public boolean EndOfStream()
  {
    return (iCurrent == iString.length());
  }
  @Override
  public int Position()
  {
    return iCurrent;
  }
  @Override
  public void SetPosition(int aPosition)
  {
    iCurrent = aPosition;
  }

  StringBuffer iString;
  int iCurrent;
}

