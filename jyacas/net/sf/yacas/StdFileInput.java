package net.sf.yacas;


import java.io.*;


class StdFileInput extends StringInput
{
    public StdFileInput(String fname, InputStatus status) throws Exception {
        super(new StringBuffer(), status);
        
        FileInputStream stream = new FileInputStream(fname);
        InputStreamReader reader = new InputStreamReader(stream, "UTF-8");
        int c;
        while (true) {
            c = reader.read();
            if (c == -1) {
                break;
            }
            iString.append((char) c);
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
