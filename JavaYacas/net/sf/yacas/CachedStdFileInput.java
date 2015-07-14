package net.sf.yacas;


/** CachedStdFileInput : input from stdin */
class CachedStdFileInput extends LispInput
{
    public CachedStdFileInput(InputStatus aStatus)
    {
      super(aStatus);
      Rewind();
    }
    @Override
    public char Next() throws Exception
    {
      int c = Peek();
      iCurrentPos++;
      if (c == '\n')
        iStatus.NextLine();
      return (char)c;
    }
    @Override
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
    @Override
    public boolean EndOfStream()
    {
      return false;
    }
    public void Rewind()
    {
      iBuffer = new StringBuffer();
      iCurrentPos = 0;
    }
    @Override
    public int Position()
    {
      return iCurrentPos;
    }
    @Override
    public void SetPosition(int aPosition)
    {
      iCurrentPos = aPosition;
    }

    StringBuffer iBuffer;
    int iCurrentPos;
}
