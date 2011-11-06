package net.sf.yacas;


import java.io.*;
import java.net.*;

class JarInputFile extends StringInput
{
  public JarInputFile(String aFileName, InputStatus aStatus) throws Exception
  {
    super(new StringBuffer(),aStatus);
  URL url = new URL(aFileName);
  JarURLConnection con = (JarURLConnection) url.openConnection();
    InputStream stream = con.getInputStream();
    int c;
    while (true)
    {
      c = stream.read();
      if (c == -1)
        break;
      iString.append((char)c);
    }
  }
}
