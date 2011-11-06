package net.sf.yacas;


class StringOutput extends LispOutput
{
    public StringOutput(StringBuffer aString)
  {
    iString = aString;
  }
    public void PutChar(char aChar)
  {
    iString.append(aChar);
  }
    StringBuffer iString;
}
