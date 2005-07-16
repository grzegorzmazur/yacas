
/*
Documentation for the applet, starting it here, but has to move to main docs:
just random thoughts for now.

1) typing 'restart' on the command line restarts the system
2) You can perform initialization calls by adding parameters "initN" in the html
   code, where N is a number from 1 upwards, which have to be in consecutive order.
3) add to the history with "historyN" parameters to the applet
*/


import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.io.*;
import java.net.*;

public class ConsoleApplet extends Applet implements KeyListener, FocusListener
{
  AppletOutput out;

  /// Applet initialization
  public void init() 
  {
    setBackground(Color.white);
    setLayout (null);
    addKeyListener(this);
    addFocusListener(this);

//    addMouseListener(this);

    out = new AppletOutput(this);
    ResetInput();
  }
  boolean focusGained = false;
  public void focusGained(FocusEvent evt) 
  {
    focusGained = true;
    repaint();
  }


  public void focusLost(FocusEvent evt) 
  {
//    focusGained = false;
//    repaint();
  }

  LispOutput stdoutput = null;
  CYacas yacas = null;
  StringBuffer outp = new StringBuffer();
  public void start()
  {
    stdoutput = new StringOutput(outp);
    yacas = new CYacas(stdoutput);
    yacas.env.iCurrentInput = new CachedStdFileInput(yacas.env.iInputStatus);

    out.println("This is Yacas version '" + CVersion.VERSION + "'.");

    out.println("Yacas is Free Software--Free as in Freedom--so you can redistribute Yacas or");
    out.println("modify it under certain conditions. Yacas comes with ABSOLUTELY NO WARRANTY.");
    out.println("See the GNU General Public License (GPL) for the full conditions.");
//TODO fixme    out.println("Type ?license or ?licence to see the GPL; type ?warranty for warranty info.");
    out.println("See http://yacas.sf.net for more information and documentation on Yacas.");
    out.println("Numeric mode: \""+BigNumber.NumericLibraryName()+"\"\n");
//TODO fixme    out.println("To exit Yacas, enter  Exit(); or quit or Ctrl-c. Type ?? for help.\n");
//TODO fixme    out.println("Or type ?function for help on a function.\n");
//TODO fixme    out.println("Type 'restart' to restart Yacas.\n");
    out.println("To see example commands, keep typing Example();\n");
//	  out.println("Yacas in Java");


    {
      String docbase = getDocumentBase().toString();
      if (docbase.substring(0,4).equals("file"))
      {
        int pos = docbase.lastIndexOf("/");
        String zipFileName = docbase.substring(0,pos+1)+"scripts.zip";
        try
        {
        //  java.util.zip.ZipFile z = new java.util.zip.ZipFile(new File(new java.net.URI("file:/Users/ayalpinkus/projects/JavaYacas/tempscripts.zip")));
          java.util.zip.ZipFile z = new java.util.zip.ZipFile(new File(new java.net.URI(zipFileName)));
          LispStandard.zipFile = z;
//          out.println("Succeeded in finding "+zipFileName);
        }
        catch(Exception e)
        {
          out.println("Failed to find scripts.zip");
          out.println(""+zipFileName+" : \n");
          out.println(e.toString());
        //  return;
        }
      }
    }


    try
    {
//      PerformRequest("Connected: version of engine is ","Atom(Version())");
      out.println("");
    }
    catch (Exception e)
    {
      out.println(e);
    } 

    int i;
    i=1;
    while (true)
    {
      String argn = "init"+i;
      String s = getParameter(argn);
      if (s == null) break;
      s = unescape(s);
      yacas.Evaluate(s);
      i++;
    }

    i=1;
    while (true)
    {
      String argn = "history"+i;
      String s = getParameter(argn);
      if (s == null) break;
      s = unescape(s);
      history[currentHistoryLine] = s;
      currentHistoryLine++;
      i++;
    }

    ResetInput();

  }
  public void stop()
  {
  }


  private String unescape(String s)
  {
    StringBuffer buf = new StringBuffer();
    int i,nr=s.length();
    for(i=0;i<nr;i++)
    {
      if (s.charAt(i) == '\'' && s.charAt(i+1) == '\'')
      {
        buf.append('\"');
        i++;
      }
      else
      {
        buf.append(s.charAt(i));
      }
    }
    return buf.toString();
  }

  private void ResetInput()
  {
    inputLine = "";
    cursorPos = 0;
    historyBrowse = currentHistoryLine;
    inputDirty = true;
  }

  /// Applet destruction
	public void destroy()
  {
  }

  public void keyPressed(KeyEvent e)
  {
    processKeyEvent(e);
  }
  public void keyTyped(KeyEvent e)
  {
//    processKeyEvent(e);
  }
  public void keyReleased(KeyEvent e)
  {
//    processKeyEvent(e);
  }

  protected void processKeyEvent(KeyEvent e)
  {
    if (KeyEvent.KEY_PRESSED == e.getID())    
    {
      if (e.VK_SHIFT == e.getKeyCode()) {return;}
      if (e.VK_CONTROL == e.getKeyCode()) {return;}
      if (e.VK_ALT == e.getKeyCode()) {return;}
      else if (e.VK_HOME == e.getKeyCode())
      {
        cursorPos = 0;  
      }
      else if (e.VK_END == e.getKeyCode())
      {
        cursorPos = inputLine.length();  
      }
      else if (e.VK_LEFT == e.getKeyCode())
      {
        if (cursorPos>0) cursorPos--;  
      }
      else if (e.VK_BACK_SPACE == e.getKeyCode())
      {
        if (cursorPos>0) 
        {
          cursorPos--;  
          inputLine = new StringBuffer(inputLine).delete(cursorPos,cursorPos+1).toString();
        }
      }
      else if (e.VK_ESCAPE == e.getKeyCode())
      {
        ResetInput();
      }
      else if (e.VK_UP == e.getKeyCode())
      {
        String prefix = inputLine.substring(0,cursorPos);
        int i = historyBrowse - 1;
        while (i > 0)
        {
          if (history[i].startsWith(prefix))
            break;
          i--;
        }
        if (i >= 0 && i != historyBrowse && history[i].startsWith(prefix))
        {
          historyBrowse = i;
          inputLine = history[historyBrowse];
        }
      }
      else if (e.VK_DOWN == e.getKeyCode())
      {

        String prefix = inputLine.substring(0,cursorPos);
        int i = historyBrowse + 1;
        while (i < currentHistoryLine)
        {
          if (history[i].startsWith(prefix))
            break;
          i++;
        }
        if (i < currentHistoryLine && history[i].startsWith(prefix))
        {
          historyBrowse = i;
          inputLine = history[historyBrowse];
        }
        else 
        {
          int pos = cursorPos;
          ResetInput();
          inputLine = prefix;
          cursorPos = pos;
        }

      }
      else if (e.VK_RIGHT == e.getKeyCode())
      {
        if (cursorPos<inputLine.length()) cursorPos++;  
      }
      else if (e.VK_ENTER == e.getKeyCode())
      {
        history[currentHistoryLine] = inputLine;
        currentHistoryLine++;
        AddLine(inputPrompt+inputLine);

        PerformRequest("Out> ",inputLine);
//        outputDirty = true;
//        inputDirty = true;
        repaint(0);
        ResetInput();
      }
      else
      {
        inputLine = new StringBuffer(inputLine).insert(cursorPos,e.getKeyChar()).toString();
        cursorPos++;
      }
      inputDirty=true;
      repaint(0,getHeight()-2*fontHeight,getWidth(),2*fontHeight);
    }
  }
  void PerformRequest(String outputPrompt,String inputLine)
  {
    boolean succeed = false;
    if (inputLine.startsWith("restart"))
    {
      stop();
      out.println("Restarting");
      start();
      return;
    }

    {
      outp.delete(0,outp.length());
      String response = yacas.Evaluate(inputLine);
      if (outp.length() > 0)
      {
        AddLinesStatic("",outp.toString());
      }
      if (yacas.iError != null)
      {
        AddLinesStatic("ERROR> ",yacas.iError);
      }
      AddLinesStatic(outputPrompt,response);
      succeed = true;
    }
    {
      if (!succeed) 
      {
        out.println("Request failed");
      }
    }
  }
  void AddLinesStatic(String prompt,String str)
  {
    String err = yacas.iError;
    int pos;
    while ((pos = str.indexOf('\n')) >=0)
    {
      AddLineStatic(prompt+str.substring(0,pos));
      str = str.substring(pos+1,str.length());
    }
    if (str.length()>0) AddLineStatic(prompt+str);
  }

  final static int nrLines =  60;
  String lines[] = new String[nrLines];
  int currentLine=0;
  void AddLine(String text)
  {
    AddLineStatic(text);
    repaint(0);
  }

  void AddLineStatic(String text)
  {
    lines[currentLine] = text;
    currentLine = (currentLine+1)%nrLines;
    outputDirty = true;
  }

  /// Drawing current view
  Image yacasLogo = null;
	public void paint (Graphics g) 
  {
    if (!focusGained)
    {
      Dimension d = getSize();
      String str = "Tap here to start";
      if (yacasLogo == null)
      {
        try
        {
          String fname = getDocumentBase().toString();
          int ind = fname.lastIndexOf("/");
          if (ind >0)
          {
            fname = fname.substring(0,ind+1)+"yacas.gif";
            yacasLogo = getImage(new URL(fname));
          }
        }
        catch (Exception e)
        {
        }
      }
      if (yacasLogo != null)
        g.drawImage(yacasLogo,(d.width-yacasLogo.getWidth(this))/2,0,Color.WHITE,this);
      else
        str = getDocumentBase().toString();
      Font font = new Font("times", Font.BOLD + Font.PLAIN, 18);
      java.awt.geom.Rectangle2D m = g.getFontMetrics().getStringBounds(str,g);
      int x = (int)((d.width-m.getWidth())/2);
      int y = (d.height-18)/2;
      g.setColor(Color.blue);
      g.setFont(font);
      g.drawString(str, x, y);
      return;
    }


    FontMetrics metrics = getFontMetrics(font);

// to always redraw everything, make the whole canvas dirty
    inputDirty = outputDirty = true;

		g.setColor(Color.white);
    int yfrom = 0;
    int yto = getHeight()-1;
    if (!outputDirty)
      yfrom += getHeight()-2*fontHeight;
    if (!inputDirty)
      yto -= 2*fontHeight;
      
    g.clearRect(0,yfrom,getWidth(),yto);
		g.setColor(Color.black);
    g.drawRect(0,0,getWidth()-1,getHeight()-1);
		g.setColor(Color.blue);
		g.setFont(font);
    //fontHeight*nrLines + offset = getHeight();

    int y=getHeight()-fontHeight*(nrLines+1);
    int i;
    if (outputDirty)
    {
      for (i=0;i<nrLines;i++)
      {
        if (y+fontHeight>0)
        {
          int index = (currentLine+i)%nrLines;
          if (lines[index] != null) 
            g.drawString(lines[index], inset, y);
        }
        y+=fontHeight;
      }
    }
    else
    {
      y+=nrLines*fontHeight;
    }
    outputDirty = false;
    if (inputDirty)
    {
      if (y+fontHeight>0)
      {
        int promptLength = metrics.stringWidth(inputPrompt);
        g.setColor(Color.red);
        g.drawString(inputPrompt, inset, y);
        g.drawString(inputLine, inset+promptLength, y);
        int cursorLocation = promptLength;
        for (i=0;i<cursorPos;i++)
        {
          cursorLocation += metrics.charWidth(inputLine.charAt(i));
        }
        g.drawLine(inset+cursorLocation,y,inset+cursorLocation,y-fontHeight);
      }
    }
    inputDirty=false;
  }
  String inputLine  = new String();
  int cursorPos = 0;
  final int inset = 5;
  
  final static String inputPrompt = "In> ";
  final static String outputPrompt = "Out> ";

  static final int fontHeight = 12;
	private Font font = new Font("Monaco", /*Font.ITALIC + Font.BOLD*/Font.PLAIN, fontHeight);
//	private Font font = new Font("times", /*Font.ITALIC + Font.BOLD*/Font.PLAIN, fontHeight);
//	private Font font = new Font("serif", /*Font.ITALIC + Font.BOLD*/Font.PLAIN, fontHeight);

  private static final int nrHistoryLines = 50;
  private String history[] = new String[nrHistoryLines]; 
  int currentHistoryLine = 0;
  int historyBrowse = 0;

  boolean inputDirty = true;
  boolean outputDirty = true;


  class AppletOutput 
  {
    public AppletOutput(ConsoleApplet aApplet)
    {
      iApplet = aApplet;
    }
    ConsoleApplet iApplet;
    public void write(int c) throws IOException
    {
      if (c == '\n')
      {
        iApplet.AddLineStatic(buffer.toString());
        buffer = new StringBuffer();
      }
      else
      {
        buffer.append((char)c);
      }
    }
    public void print(String s) 
    {
      try
      {
        int i,nr;
        nr = s.length();
        for (i=0;i<nr;i++)
        {
          write(s.charAt(i));
        }
      }
      catch (IOException e)
      {
      }
    }
    public void println(Exception e) 
    {
      println(e.getMessage());
    }
    public void println(String s) 
    {
      print(s);
      print("\n");
    }
    StringBuffer buffer = new StringBuffer();
  }

}

