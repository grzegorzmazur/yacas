package net.sf.yacas;


import java.io.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.awt.Desktop;

public class YacasConsole extends Thread
{
    static String readLine(InputStream stream) {
        InputStreamReader reader = new InputStreamReader(stream);
        StringBuilder line = new StringBuilder();
        try {
            int c = reader.read();
            while (c != '\n') {
                line.append((char) c);
                c = reader.read();
            }
        } catch (Exception e) {
            System.out.println(e.toString());
        }
        return line.toString();
    }

  static boolean quitting = false;

  public static void main(String[] argv) throws IOException
  {
    String defaultDirectory = null;
    String archive = null;
    String expression = null;

    {
      java.net.URL detectURL = java.lang.ClassLoader.getSystemResource("scripts/yacasinit.ys");

      if (detectURL != null)
      {
        String path = detectURL.getPath();
        if (path.contains("!"))
            archive = path.substring(0, path.lastIndexOf('!'));
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
      else if (argv[i].equals("--archive"))
      {
        i++;
        archive = argv[i];
      }
      else if (argv[i].equals("-i"))
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

    Writer out = new OutputStreamWriter(System.out, "UTF-8");
    CYacas yacas = new CYacas(out);
    yacas.env.iCurrentInput = new CachedStdFileInput(yacas.env.iInputStatus);

    try
    {
        if (archive != null) {
            String zipFileName = archive;
            java.util.zip.ZipFile z = new java.util.zip.ZipFile(new File(new java.net.URI(zipFileName)));
            LispStandard.zipFile = z;
        }
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


    out.write("This is Yacas version '" + CVersion.VERSION + "'.\n");

    out.write("Yacas is Free Software--Free as in Freedom--so you can redistribute Yacas or\n");
    out.write("modify it under certain conditions. Yacas comes with ABSOLUTELY NO WARRANTY.\n");
    out.write("See the GNU Lesser General Public License (LGPL) version 2.1 or (at your\n");
    out.write("discretion) any later version for the full conditions.\n");
    out.write("Type ?license or ?licence to see the LGPL version 2.1;\n");
    out.write("type ?warranty for warranty info.\n");
    out.write("See http://grzegorzmazur.github.io/yacas/ for more information on yacas\n");
    out.write("and documentation.\n");
    out.write("Type ?? for help. Or type ?function for help on a function.\n\n");

    out.write("To exit Yacas, enter  Exit(); or quit or Ctrl-c.\n");
    //    System.out.println("Type 'restart' to restart Yacas.\n");

    out.write("To see example commands, keep typing Example();\n");

    out.write("Yacas in Java\n");

    while (!quitting) {

        out.write("In> ");
        out.flush();
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

        out.write("Out> " + rs + "\n");
        out.flush();
    }
  }
}
