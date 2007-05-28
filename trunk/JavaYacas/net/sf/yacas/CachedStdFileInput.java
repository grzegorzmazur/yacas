package net.sf.yacas;


import java.io.*;

/** CachedStdFileInput : input from stdin */
class CachedStdFileInput extends LispInput
{
    public CachedStdFileInput(InputStatus aStatus)
    {
      super(aStatus);
      Rewind();
    }
    public char Next() throws Exception
    {
      int c = Peek();
      iCurrentPos++;
      if (c == '\n')
        iStatus.NextLine();
      return (char)c;
    }
    public char Peek() throws Exception
    {
      if (iCurrentPos == iBuffer.length())
      {
        int newc;
        newc = System.in.read();
        iBuffer.append((char)newc);
        while (newc != '\n')
        {
          newc = System.in.read();
          iBuffer.append((char)newc);
        }
      }
      return iBuffer.charAt(iCurrentPos);
    }
    public boolean EndOfStream()
    {
      return false;
    }
    public void Rewind()
    {
      iBuffer = new StringBuffer();
      iCurrentPos = 0;
    }
    public StringBuffer StartPtr()
    {
      return iBuffer;
    }
    public int Position()
    {
      return iCurrentPos;
    }
    public void SetPosition(int aPosition)
    {
      iCurrentPos = aPosition;
    }
 
    StringBuffer iBuffer;
    int iCurrentPos;
}
