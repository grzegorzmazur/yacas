package net.sf.yacas;


import java.io.*;
import java.util.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.awt.Desktop;

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
    String expression = null;

    {
      java.net.URL detectURL = java.lang.ClassLoader.getSystemResource("yacasinit.ys");
      if (detectURL != null)
      {
        String detect = detectURL.getPath();
        archive = detect.substring(0, detect.lastIndexOf('!'));
      }
      else
      {
          // FIXME: report the error
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
      if (argv[i].equals("-i"))
      {
        i++;
        expression = argv[i];
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
      String zipFileName = archive;
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
      if (scriptsToRun == argv.length && expression == null)
        System.out.println("Out> "+result);
    }
    if (expression != null)
    {
        String result = yacas.Evaluate(expression);
        return;
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
    System.out.println("Type ?license or ?licence to see the GPL; type ?warranty for warranty info.");
    System.out.println("See http://yacas.sf.net for more information and documentation on Yacas.");
    System.out.println("Type ?? for help. Or type ?function for help on a function.\n");

    System.out.println("To exit Yacas, enter  Exit(); or quit or Ctrl-c.\n");
    //    System.out.println("Type 'restart' to restart Yacas.\n");

    System.out.println("To see example commands, keep typing Example();\n");

    System.out.println("Yacas in Java");

    while (!quitting) {

        System.out.print("In> ");
        String input = readLine(System.in);

        String rs = "True";

        if (input.trim().equals("quit")) {
            quitting = true;
        } else if (input.trim().startsWith("?")) {
            String key = input.trim().substring(1);

            String prefix = "http://yacas.sourceforge.net/";

            try {
                URI uri = new URI(prefix + "ref.html?" + key);

                if (key.equals("license") || key.equals("licence"))
                    uri = new URI(prefix + "refprogchapter9.html");
                else if (key.equals("warranty"))
                    uri = new URI(prefix + "refprogchapter9.html#c9s2");
                else if (key.equals("?"))
                    uri = new URI(prefix + "refmanual.html");

                try {
                    Desktop.getDesktop().browse(uri);
                } catch (IOException e) {
                    // FIXME: report the error
                    rs = "False";
                }

            } catch (URISyntaxException e) {
                // it's a cold night in Hell
                rs = "False";
            }
        } else {
            rs = yacas.Evaluate(input);
        }

        System.out.println("Out> " + rs);
    }
  }
}
