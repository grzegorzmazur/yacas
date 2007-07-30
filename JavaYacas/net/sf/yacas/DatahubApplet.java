
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
  public String getProgram()
  {
    switch (currentProgram)
    {
    case 2:
      return tutorialProgram.codeBody;
    case 1:
      return journalProgram.codeBody;
    case 0:
    default:
      return consoleProgram.codeBody;
    }
  }

  public String getExample()
  {
    switch (currentProgram)
    {
    case 2:
      return tutorialProgram.getExample();
    case 1:
      return journalProgram.getExample();
    case 0:
    default:
      return consoleProgram.getExample();
    }
  }


  public String getArticle()
  {
    switch (currentProgram)
    {
    case 2:
      return tutorialProgram.iArticle;
    case 1:
      return journalProgram.iArticle;
    case 0:
    default:
      return consoleProgram.iArticle;
    }
  }
  public String getArticleBody()
  {
    switch (currentProgram)
    {
    case 2:
      return tutorialProgram.articleBody;
    case 1:
      return journalProgram.articleBody;
    case 0:
    default:
      return consoleProgram.articleBody;
    }
  }
  
  public void setArticle(String p)
  {
    switch (currentProgram)
    {
    case 2:
      tutorialProgram.SetArticle(p); break;
    case 1:
      journalProgram.SetArticle(p); break;
    case 0:
    default:
      consoleProgram.SetArticle(p); break;
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
    else if (mode.equals("tutorial"))
    {
      currentProgram = 2;
    }
  }

  String readArticleFromFile(String urlStr)
  {
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

    String prog = "";
    try
    {
      URL url = new URL(docbase);
      BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
 
      if (in != null)
      {
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
        in.close();
      }
    }
    catch(Exception e) 
    {
    }
    return prog;
  }

  public void setArticleFromFile(String urlStr)
  {
    setArticle(readArticleFromFile(urlStr));
  }
  
  static Article consoleProgram = new Article("You can start entering your own program between the code delimiters below.\n\n{{code:\n:code}}\n");
  static Article journalProgram = new Article("No article loaded yet.");
  static Article tutorialProgram = new Article("{{code: Echo(\"Welcome to the Yacas tutorial!\"); :code}}");
  static int currentProgram = 0;
}
 
