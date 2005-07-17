
/*

1
2

foo():=\
[\
  For(i:=0,i<10,i++)\
  [\
    Echo(i);\
  ];\
];


Documentation for the applet, starting it here, but has to move to main docs:
just random thoughts for now.

1) typing 'restart' on the command line restarts the system
2) You can perform initialization calls by adding parameters "initN" in the html
   code, where N is a number from 1 upwards, which have to be in consecutive order.
3) add to the history with "historyN" parameters to the applet
*/


import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.Toolkit;import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.io.*;
import java.net.*;

public class ConsoleApplet extends Applet implements KeyListener, FocusListener, ClipboardOwner
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
    
    String hintsfilename = getDocumentBase().toString() + ".hints";
    LoadHints(hintsfilename);
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

  public void lostOwnership(Clipboard clipboard, Transferable contents)
  {
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
    if (inputLine.length()>0)
      if (inputLine.charAt(inputLine.length()-1) != '\\')
        gatheredMultiLine = "";
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


  public void setClipboardContents( String aString )
  {
    StringSelection stringSelection = new StringSelection( aString );
    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
    clipboard.setContents( stringSelection, this );
  }
  public String getClipboardContents() 
  {
    String result = "";
    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
    //odd: the Object param of getContents is not currently used
    Transferable contents = clipboard.getContents(null);
    boolean hasTransferableText = (contents != null) &&
                                  contents.isDataFlavorSupported(DataFlavor.stringFlavor);
    if ( hasTransferableText ) 
    {
      try 
      {
        result = (String)contents.getTransferData(DataFlavor.stringFlavor);
      }
      catch (java.awt.datatransfer.UnsupportedFlavorException ex)
      {
        //highly unlikely since we are using a standard DataFlavor
        System.out.println(ex);
      }
      catch (IOException ex) 
      {
        System.out.println(ex);
      }
    }
    return result;
  }

  protected void processKeyEvent(KeyEvent e)
  {

    if ((e.getModifiers() & InputEvent.CTRL_MASK) == InputEvent.CTRL_MASK)
    {
      if (KeyEvent.KEY_PRESSED != e.getID())
        return;
      if (e.getKeyCode() == (int)'C')
      {
        //out.println("Copy");
        setClipboardContents( gatheredMultiLine+inputLine );
      }
      else if (e.getKeyCode() == (int)'V')
      {
        try
        {
          String toInsert = getClipboardContents();
          if (toInsert != null)
          {
            int cr = toInsert.indexOf('\n');
            while (cr >= 0)
            {
              inputLine = inputLine+toInsert.substring(0,cr);
              toInsert = toInsert.substring(cr+1,toInsert.length());
              cr = toInsert.indexOf('\n');
//System.out.println("");
              history[currentHistoryLine] = inputLine;
              currentHistoryLine++;
              AddLine(inputPrompt+inputLine);
              if (inputLine.charAt(inputLine.length()-1) == '\\')
                gatheredMultiLine = gatheredMultiLine + inputLine.substring(0,inputLine.length()-1);
              else
                PerformRequest("Out> ",gatheredMultiLine+inputLine);
              ResetInput();
            }
            inputLine = inputLine+toInsert;
            RefreshHintWindow();
            repaint();
            return;
          }
          //  out.println("Paste");
//          out.println(toInsert);
        }
        catch (Exception ex)
        {
        }
      }
      else
      {
        return;
      }
    }

    if (KeyEvent.KEY_PRESSED == e.getID())    
    {
      if (e.VK_SHIFT == e.getKeyCode()) {return;}
      if (e.VK_CONTROL == e.getKeyCode()) {return;}
      if (e.VK_ALT == e.getKeyCode()) {return;}
      else if (e.VK_HOME == e.getKeyCode())
      {
        cursorPos = 0;  
      }
/*Does not seem to work?
      else if (e.VK_COPY == e.getKeyCode())
      {
        System.out.println("COPY");
      }
      else if (e.VK_PASTE == e.getKeyCode())
      {
        System.out.println("PASTE");
      }
*/
      else if (e.VK_END == e.getKeyCode())
      {
        cursorPos = inputLine.length();  
      }
      else if (e.VK_LEFT == e.getKeyCode())
      {
        if (cursorPos>0) 
        {
          cursorPos--;  
          RefreshHintWindow();
          repaint();
          return;
        }
      }
      else if (e.VK_BACK_SPACE == e.getKeyCode())
      {
        if (cursorPos>0) 
        {
          cursorPos--;  
          inputLine = new StringBuffer(inputLine).delete(cursorPos,cursorPos+1).toString();
          RefreshHintWindow();
          repaint();
          return;
        }
      }
      else if (e.VK_ESCAPE == e.getKeyCode())
      {
        if (hintWindow != null)
        {
          hintWindow = null;
        }
        else
        {
          ResetInput();
        }
        repaint();
        return;
      }
      else if (e.VK_UP == e.getKeyCode())
      {
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
      }
      else if (e.VK_DOWN == e.getKeyCode())
      {
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
      }
      else if (e.VK_RIGHT == e.getKeyCode())
      {
        if (cursorPos<inputLine.length()) 
        {
          cursorPos++;  
          RefreshHintWindow();
          repaint();
          return;
        }
      }
      else if (e.VK_ENTER == e.getKeyCode())
      {
        history[currentHistoryLine] = inputLine;
        currentHistoryLine++;
        AddLine(inputPrompt+inputLine);
        if (inputLine.charAt(inputLine.length()-1) == '\\')
          gatheredMultiLine = gatheredMultiLine + inputLine.substring(0,inputLine.length()-1);
        else
          PerformRequest("Out> ",gatheredMultiLine+inputLine);
        ResetInput();
        RefreshHintWindow();
        repaint(0);
      }
      else
      {
        int c = (int)e.getKeyChar();
        if (c>=32 && c < 128)
        {
          inputLine = new StringBuffer(inputLine).insert(cursorPos,e.getKeyChar()).toString();
          cursorPos++;
          RefreshHintWindow();
        }
      }
      inputDirty=true;
      repaint();//0,getHeight()-2*fontHeight,getWidth(),2*fontHeight);
    }
  }
  void PerformRequest(String outputPrompt,String inputLine)
  {
    boolean succeed = false;
    if (inputLine.equals("restart"))
    {
      stop();
      out.println("Restarting");
      start();
      return;
    }
    
    if (inputLine.equals("cls"))
    {
      int i;
      for (i=0;i<nrLines;i++) lines[i] = null;
      outputDirty = true;
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
    
    if (hintWindow != null)
    {
//System.out.println("Rendering hints");
      YacasGraphicsContext context = new YacasGraphicsContext(g,0,0);
      context.SetFontSize(1,12);  
      int nr_total_lines = 1; //nrLines;
      Dimension d = getSize();
//      hintWindow.draw(100,100,context);
      hintWindow.draw(5,(int)(d.getHeight()-context.FontHeight()-nr_total_lines*context.FontHeight()),context);
    }
    
    inputDirty=false;
  }
  String inputLine  = new String();
  String gatheredMultiLine = new String();

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


  HintWindow hintWindow = null;
  Hints the_hints = new Hints();

  void LoadHints(String filename)
  {
//    out.println("hints file : ["+filename+"]");
    CDataReader file = new CDataReader();
    int opened = 0;
    try 
    {
      URL url = new URL(filename);
      opened = file.Open(url);
    }
    catch (Exception e)
    {
    }
    if (opened != 0)
    {
//      out.println("hints opened successfully");
      String line = file.ReadLine();
      String[] tokens = new String[16];
      int nrTokens = 0;
      while (line != null)
      {
      
//        System.out.println("LINE : "+line);
        if (line.substring(0,2).equals("~~"))
          break;
        int i=0;
        nrTokens = 0;
        while (i<line.length())
        {
          int start = i;
          while (line.charAt(i) != '~') i++;
          tokens[nrTokens] = line.substring(start,i);
          nrTokens++;
          i++;
        }
//        System.out.println("DIGITS : "+tokens[1]);
        if (nrTokens>4)
        {
          HintItem hi = new HintItem();
          hi.digits = tokens[1];
          hi.base = tokens[2];
          hi.hint = tokens[3];
          hi.description = tokens[4];
          the_hints.hintTexts[the_hints.nrHintTexts] = hi;
          the_hints.nrHintTexts++;
        }

        line = file.ReadLine();
      }
      file.Close();
    }
    else
    {
      out.println("could not read hints");
    }
  }

  HintWindow CreateHints(int fontsize)
  {
    return new HintWindow(fontsize);
  }
  
  void AddHintLine(HintWindow hints, String aText, String aDescription)
  {
      hints.AddLine(aText);
      if (aDescription.length() > 0)
        hints.AddDescription(aDescription);
  }
  
  
  HintWindow TryToHint(String text, int length)
  {
    HintWindow hints = null;
    int nrhints = the_hints.nrHintTexts;
//System.out.println("nrhints = "+nrhints);
    int i,start;
    start = 0;//hoffsets[(unsigned char)text[0]];
    if (start<0)
        return null;
    for (i = start;i<nrhints;i++)
    {
      if (text.charAt(0) > the_hints.hintTexts[i].base.charAt(0))
      {
        continue;
      }
      if (text.charAt(0) < the_hints.hintTexts[i].base.charAt(0))
      {
        continue;
      }
//System.out.println("base "+the_hints.hintTexts[i].base);
      int baselen = the_hints.hintTexts[i].base.length();
      if (length == baselen)
      {
        if (text.substring(0,baselen).equals(the_hints.hintTexts[i].base))
        {
//System.out.println("Adding hint line");
          if (hints == null)
              hints = CreateHints(12 /*iDefaultFontSize*/);
          AddHintLine(hints, the_hints.hintTexts[i].hint,the_hints.hintTexts[i].description);
        }
      }
    }
    return hints;
  }

  int search_start = 0;
  private String FindWord(Hints aHints, String current_word)
  {
//System.out.println("search_start = "+search_start);
    int nr = current_word.length();
    if (nr>0)
    {
      int i;
      while (true)
      {
        for (i=search_start;i<aHints.nrHintTexts;i++)
        {
          if (nr <= (aHints.hintTexts[i].digits).length()-1 && 
              current_word.equals(aHints.hintTexts[i].base.substring(0,nr)))
          {
            search_start = i;
            return aHints.hintTexts[i].base;
          }
        }
        if (i == aHints.nrHintTexts && search_start != 0)
        {
          search_start = 0;
//          goto REDO;
        }
        else
        {
          break;
        }
      }
    }
    return "";
  }

  void RefreshHintWindow()
  {
    int ito = cursorPos;

    while (true)
    {
      if (ito==inputLine.length())
        break;
      if (!LispTokenizer.IsAlpha(inputLine.charAt(ito)))
        break;
      ito++;
    }
    if(ito>0)
    {
      int c = inputLine.charAt(ito-1);
      if (c == ',' || c == ')')
      {
        int braces = -1;
        if (c == ')')  
        {
          ito--;
          braces = -2;
        }
        while (braces!=0)
        {
          if (ito<=0)
            break;
          if (inputLine.charAt(ito-1) == '(') braces++;
          if (inputLine.charAt(ito-1) == ')') braces--;
          ito--;
        }
      }
    }
    if(ito>0)
    {
      if (inputLine.charAt(ito-1) == '(')
      {
        ito--;
      }
    }
    if (ito == 0)
    {
      while (true)
      {
        if (ito==cursorPos)
          break;
        if (!LispTokenizer.IsAlpha(inputLine.charAt(ito)))
          break;
        ito++;
      }
    }
    int ifrom = ito;
    while (true)
    {
      if (ifrom == 0)
        break;
      if (!LispTokenizer.IsAlpha(inputLine.charAt(ifrom-1)))
        break;
      ifrom--;
    }

    String word = "";//inputLine.toString();
    if (ito>ifrom)
    {
      word = inputLine.substring(ifrom,ito);
//System.out.println("ifrom = "+ifrom+" ito = "+ito);
//System.out.println("word = "+word);
    }

    String str = FindWord(the_hints, word);
    if (str.length()>0)
      hintWindow = TryToHint(str, str.length());
    else
      hintWindow = null;
  }
}

