package net.sf.yacas;


import java.io.*;
import java.util.*;

public class YacasConsole extends Thread
{
    static String readLine(InputStream aStream)
  {
    StringBuffer line = new StringBuffer();
    try
    {
      int c = aStream.read();
      while (c != '\n')
      {
      line.append((char)c);
      c = aStream.read();
      }
    }
    catch (Exception e)
    {
      System.out.println(e.toString());
    }
    return line.toString();
  }
  static boolean quitting = false;

  public static void main(String[] argv)
  {
    String defaultDirectory = null;
    String archive = "";

    {
      java.net.URL detectURL = java.lang.ClassLoader.getSystemResource("yacasinit.ys");
      if (detectURL != null)
      {
        String detect = detectURL.getPath(); // file:/home/av/src/lib/yacas.jar!/yacasinit.ys
        archive = detect.substring(0, detect.lastIndexOf('!')); // file:/home/av/src/lib/yacas.jar
//System.out.println("Found archive ["+archive+"]");
      }
      else
      {
//System.out.println("Archive not found!!!!");
      }
    }
    int i=0;
    while (i<argv.length)
    {
      if (argv[i].equals("--rootdir"))
      {
        i++;
        defaultDirectory = argv[i];
      }
      if (argv[i].equals("--archive"))
      {
        i++;
        archive = argv[i];
      }
      else
      {
        break;
      }
      i++;
    }
    int scriptsToRun = i;


    StdFileOutput stdoutput = new StdFileOutput(System.out);
    CYacas yacas = new CYacas(stdoutput);
    yacas.env.iCurrentInput = new CachedStdFileInput(yacas.env.iInputStatus);

    try
    {
      String zipFileName = archive;//"file:/Users/ayalpinkus/projects/JavaYacas/yacas.jar";
      java.util.zip.ZipFile z = new java.util.zip.ZipFile(new File(new java.net.URI(zipFileName)));
      LispStandard.zipFile = z;
    }
    catch(Exception e)
    {
      System.out.println("Failed to find yacas.jar"+e.toString());
    }


    if (defaultDirectory != null)
    {
      String toEvaluate = "DefaultDirectory(\""+defaultDirectory+"\");";
      String result = yacas.Evaluate(toEvaluate);
      if (scriptsToRun == argv.length)
        System.out.println("Out> "+result);
    }
    {
      String result = yacas.Evaluate("Load(\"yacasinit.ys\");");
      if (scriptsToRun == argv.length)
        System.out.println("Out> "+result);
    }
    if (scriptsToRun < argv.length)
    {
      for (;scriptsToRun<argv.length;scriptsToRun++)
      {
        yacas.Evaluate("Load(\""+argv[scriptsToRun]+"\");");
      }
      return;
    }


    System.out.println("This is Yacas version '" + CVersion.VERSION + "'.");

    System.out.println("Yacas is Free Software--Free as in Freedom--so you can redistribute Yacas or");
    System.out.println("modify it under certain conditions. Yacas comes with ABSOLUTELY NO WARRANTY.");
    System.out.println("See the GNU General Public License (GPL) for the full conditions.");
//TODO fixme    System.out.println("Type ?license or ?licence to see the GPL; type ?warranty for warranty info.");
    System.out.println("See http://yacas.sf.net for more information and documentation on Yacas.");

    System.out.println("To exit Yacas, enter  Exit(); or quit or Ctrl-c.\n");
/*TODO fixme
    System.out.println("Type ?? for help. Or type ?function for help on a function.\n");
    System.out.println("Type 'restart' to restart Yacas.\n");
*/
    System.out.println("To see example commands, keep typing Example();\n");

//yacas.Evaluate("BubbleSort(N(PSolve(x^3-3*x^2+2*x,x)), \"<\");");

    System.out.println("Yacas in Java");
    while (!quitting)
    {
      System.out.print("In> ");
      String input =  readLine(System.in);
      String rs = yacas.Evaluate(input);
      System.out.println("Out> "+rs);
      if (input.equals("quit")) quitting = true;
    }
  }
}
