
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
  public void init()
  {
    String programMode = getParameter("programMode");
    if (programMode != null)
    {
      setProgramMode(programMode);
//System.out.println("Setting program mode "+programMode);
      String articleFromFile = getParameter("articleFromFile");
      if (articleFromFile != null)
      {
//System.out.println("Loading article "+articleFromFile);
        setArticleFromFile(articleFromFile);
      }

      String article = getParameter("article");
      if (article != null)
      {
//System.out.println("Loading article "+articleFromFile);
        setArticle(article);
      }


//setArticle

    }
  }

  Article currentArticle()
  {
    switch (currentProgram)
    {
    case 2:
      return tutorialProgram;
    case 1:
      return journalProgram;
    case 0:
    default:
      return consoleProgram;
    }
  }
  public String getProgram()
  {
    synchronized(consoleProgram)
    {
      return currentArticle().codeBody;
    }
  }

  public String getExample()
  {
    synchronized(consoleProgram)
    {
      return currentArticle().getExample();
    }
  }

  public String getTestcode()
  {
    synchronized(consoleProgram)
    {
      return currentArticle().getTestcode();
    }
  }


  public String getNrExamples()
  {
    synchronized(consoleProgram)
    {
      return currentArticle().getNrExamples();
    }
  }


  public String getArticle()
  {
    synchronized(consoleProgram)
    {
      return currentArticle().iArticle;
    }
  }
  public String getArticleBody()
  {
    synchronized(consoleProgram)
    {
      return currentArticle().articleBody;
    }
  }
  
  public void setArticle(String p)
  {
//System.out.println("article:\n"+p);
    synchronized(consoleProgram)
    {
      currentArticle().SetArticle(p);
    }
  }

  public void setProgramMode(String mode)
  {
    synchronized(consoleProgram)
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

  // To make sure it is not reloaded each time
  static String prevLoadedArticle = "";

  public void setArticleFromFile(String urlStr)
  {
    if (!prevLoadedArticle.equals(urlStr))
    {
      prevLoadedArticle = urlStr;
      synchronized(consoleProgram)
      {
        setArticle(readArticleFromFile(urlStr));
      }
    }
  }
  
  static Article consoleProgram = new Article("You can start entering your own program between the code delimiters below.\n\n{{code:\n:code}}\n");
  static Article journalProgram = new Article("No article loaded yet.");
  static Article tutorialProgram = new Article("{{code: Echo(\"Welcome to the Yacas tutorial!\"); :code}}");
  static int currentProgram = 0;
}
 