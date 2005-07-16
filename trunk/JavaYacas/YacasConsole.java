
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
/*
try
{
  java.util.zip.ZipFile z = new java.util.zip.ZipFile(new File(new java.net.URI("file:/Users/ayalpinkus/projects/JavaYacas/tempscripts.zip")));
  java.util.zip.ZipEntry e = z.getEntry("tempscripts/init.ys");
  InputStream s = z.getInputStream(e);
  int i;
  char c;
  for (i=0;i<5;i++)
  {
    c = (char)s.read();
  }
}
catch(Exception e)
{
  System.out.println(e.toString());
  return;
}
*/
/*
try
{
  java.net.URL url = new java.net.URL("jar:file:/Users/ayalpinkus/projects/JavaYacas/temp.zip!/temp/test.ys");
  java.net.JarURLConnection jarConnection = (java.net.JarURLConnection)url.openConnection();
  InputStream s = jarConnection.getInputStream();
  int i;
  char c;
  for (i=0;i<5;i++)
  {
    c = (char)s.read();
  }
}
catch(Exception e)
{
  System.out.println(e.toString());
}
*/



    String defaultDirectory = null;
    String archive = "file:/Users/ayalpinkus/projects/yacas/JavaYacas/scripts.zip";
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
    //  java.util.zip.ZipFile z = new java.util.zip.ZipFile(new File(new java.net.URI("file:/Users/ayalpinkus/projects/JavaYacas/tempscripts.zip")));
      String zipFileName = archive;//"file:/Users/ayalpinkus/projects/JavaYacas/scripts.zip";
      java.util.zip.ZipFile z = new java.util.zip.ZipFile(new File(new java.net.URI(zipFileName)));
      LispStandard.zipFile = z;
//      System.out.println("Succeeded in finding "+zipFileName);
    }
    catch(Exception e)
    {
      System.out.println("Failed to find scripts.zip"+e.toString());
    //  return;
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


    System.out.println("Numeric mode: \""+BigNumber.NumericLibraryName()+"\"\n");
//TODO fixme    System.out.println("To exit Yacas, enter  Exit(); or quit or Ctrl-c. Type ?? for help.\n");
//TODO fixme    System.out.println("Or type ?function for help on a function.\n");
//TODO fixme    System.out.println("Type 'restart' to restart Yacas.\n");
    System.out.println("To see example commands, keep typing Example();\n");


	  System.out.println("Yacas in Java");
	  while (!quitting)
	  {
		  System.out.print("In>");
//      String input = "a:=-2;";//"[Precision(40);1.0;];";//"[Set(pat,PatternCreate(foo(_a,_b),True));PatternMatches(pat,bar(x,y));];";//"[Local(a);Set(a,2);MathAdd(a,1);];";//"Set(a,2);";//"1.0e3";//"MathNegate(1.1);";//"IsAtom({a,b,c});";// readLine(System.in);
		  String input =  readLine(System.in);

      String rs = yacas.Evaluate(input);
		  System.out.println("Out> "+rs);
		  if (input.equals("quit")) quitting = true;
	  }
	}
}
