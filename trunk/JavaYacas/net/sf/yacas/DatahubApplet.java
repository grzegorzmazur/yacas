
package net.sf.yacas;

import java.applet.*;
import java.net.*;
import java.io.*;


/* This little applet should facilitate communication between Java and Javascript.
 * The idea is to allow Javascript to set data in this applet at various times, and
 * for the main Yacas console to then get that data at startup, when it is loaded.
 * 
 * The Yacas console thus does not need to be loaded always, but this applet should
 * always be there.
 */
 
 
public class DatahubApplet extends Applet
{
  public String getProgramToLoad()
  {
    switch (currentProgram)
    {
    case 1:
      return journalProgram;
    case 0:
    default:
      return consoleProgram;
    }
  }
  public void setProgramToLoad(String p)
  {
    switch (currentProgram)
    {
    case 1:
      journalProgram = p; break;
    case 0:
    default:
      consoleProgram = p; break;
    }
  }

  public void setProgramMode(String mode)
  {
    if (mode.equals("console"))
    {
      currentProgram = 0;
    }
    else if (mode.equals("journal"))
    {
      currentProgram = 1;
    }
  }

  public void setProgramToLoadFromFile(String urlStr)
  {
    setProgramToLoad("/* Program was not loaded */\nWriteString(\"Program was not loaded.\");");

    String docbase = getDocumentBase().toString();
    int pos = docbase.lastIndexOf('/');
    if (pos > -1)
    {
      docbase = docbase.substring(0,pos+1);
    }
    else
    {
      docbase = "";
    }
    docbase = docbase+urlStr;

    try
    {
      URL url = new URL(docbase);
//TODO remove?      DataInputStream in = new DataInputStream(url.openStream());
     BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
 
      if (in != null)
      {
        String prog = "";
        setProgramToLoad(prog);
        while (true)
        {
          try
          {
            String mark = in.readLine();
            if (mark == null)
              break;
            prog = prog + mark + "\n";
          }
          catch (Exception e)
          {
          }
        }
        setProgramToLoad(prog);
        in.close();
      }

    }
    catch(Exception e) 
    {
    }
  }
  static String consoleProgram = "/* You can start typing here to enter your own program. \nAfter you are done you can go back to the console with \nthe \"Run\" button. The code in this editor then gets loaded at startup. */\n";
  static String journalProgram = "";
  static int currentProgram = 0;
}
 
