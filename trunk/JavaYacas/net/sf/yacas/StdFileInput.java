package net.sf.yacas;


import java.io.*;


class StdFileInput extends StringInput
{
  public StdFileInput(String aFileName, InputStatus aStatus) throws Exception
  {
    super(new StringBuffer(),aStatus);
    FileInputStream stream = new FileInputStream(aFileName);
    int c;
    while (true)
    {
      c = stream.read();
      if (c == -1)
        break;
      iString.append((char)c);
    }
  }
  public StdFileInput(InputStream aStream, InputStatus aStatus) throws Exception
  {
    super(new StringBuffer(),aStatus);
    int c;
    while (true)
    {
      c = aStream.read();
      if (c == -1)
        break;
      iString.append((char)c);
    }
  }

}
