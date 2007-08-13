package net.sf.yacas;


/*

Documentation for the applet, starting it here, but has to move to main docs:
just random thoughts for now.

1) typing 'restart' on the command line restarts the system
2) You can perform initialization calls by adding parameters "initN" in the html
   code, where N is a number from 1 upwards, which have to be in consecutive order.
3) add to the history with "historyN" parameters to the applet


<B>Note:</B> to allow supporting copy-pasting from and to this applet, you need to enable
access to the clip board on your computer. On Unix-style computers this can be done
by appending the following lines to the file ~/.java.policy (create the file if it does
not exist yet):
<p>
<TT>
grant codeBase "http://www.xs4all.nl/~apinkus/*" {
permission java.awt.AWTPermission "accessClipboard";
};
</TT>
<p>
You can then copy with CTRL-c and paste with CTRL-v.


*/


import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.Toolkit;
import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.io.*;
import java.net.*;

public class ConsoleApplet extends Applet implements KeyListener, FocusListener, ClipboardOwner, MouseListener
{
  AppletOutput out;

  /// Applet initialization
  public void init()
  {
    setBackground(bkColor);
    setLayout (null);
    addKeyListener(this);
    addFocusListener(this);

    addMouseListener(this);

    out = new AppletOutput(this);
    ResetInput();
 
    String hintsfilename = getDocumentBase().toString();
    int slash = hintsfilename.lastIndexOf('/');
    if (slash >= 0)
    {
      hintsfilename = hintsfilename.substring(0,slash+1);
    }
    hintsfilename = hintsfilename + "hints.txt";
    LoadHints(hintsfilename);
  }
  boolean focusGained = false;
  public void focusGained(FocusEvent evt)
  {
    focusGained = true;
    inputDirty = true;
    outputDirty = true;
    if (!gotDatahubInit) start();
    repaint();
  }


  public void focusLost(FocusEvent evt)
  {
    focusGained = false;
    inputDirty = true;
    outputDirty = true;
    repaint();
  }

  public void mouseClicked(MouseEvent event) 
  {
  }
  public void mouseEntered(MouseEvent event) 
  {
  }
  public void mouseExited(MouseEvent event) 
  {
  }
  public void mousePressed(MouseEvent event) 
  {
  }
  public void mouseReleased(MouseEvent event) 
  {
    if (hintWindow != null)
    {
      if (matchToInsert.length() > 0)
      {
        inputLine = inputLine.substring(0,ito) + matchToInsert + inputLine.substring(ito,inputLine.length());
        cursorPos += matchToInsert.length();
        RefreshHintWindow();
        repaint();
        return;
      }
    }
  }

  public void lostOwnership(Clipboard clipboard, Transferable contents)
  {
  }

  LispOutput stdoutput = null;
  CYacas yacas = null;
  StringBuffer outp = new StringBuffer();
  public void start()
  {
    int i;
    for (i=0;i<nrLines;i++) lines[i] = null;
    outputDirty = true;

    if (false /*TODO remove loading the logo yacasLogo == null*/)
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


    stdoutput = new StringOutput(outp);
    yacas = new CYacas(stdoutput);
    yacas.env.iCurrentInput = new CachedStdFileInput(yacas.env.iInputStatus);


    if (yacasLogo != null)
    {
      lines[currentLine] = new ImageLine(yacasLogo,this);
      currentLine = (currentLine+1)%nrLines;
    }

    {
      String s;
      int bkred=255;
      int bkgrn=255;
      int bkblu=255;
      s = getParameter("bkred"); if (s != null) bkred = Integer.parseInt(s);
      s = getParameter("bkgrn"); if (s != null) bkgrn = Integer.parseInt(s);
      s = getParameter("bkblu"); if (s != null) bkblu = Integer.parseInt(s);
      bkColor = new Color(bkred,bkgrn,bkblu);
      setBackground(bkColor);
    }

    {
      Font font = new Font("helvetica", Font.PLAIN, 12);
      Color c = new Color(96,96,96);

      AddLineStatic(100, "","", font, c);
      AddLineStatic(100, "","", font, c);

      AddLineStatic(100, "","Yacas version '" + CVersion.VERSION + "'.", font, c);

//      AddLineStatic(100, "","Running from location '" + getDocumentBase() + "'.", font, c);

//      AddLineStatic(100, "","Yacas is Free Software--Free as in Freedom--so you can redistribute Yacas or", font, c);
//      AddLineStatic(100, "","modify it under certain conditions. Yacas comes with ABSOLUTELY NO WARRANTY.", font, c);
//      AddLineStatic(100, "","See the GNU General Public License (GPL) for the full conditions.", font, c);
//      AddLineStatic(100, "","Type ?license or ?licence to see the GPL; type ?warranty for warranty info.", font, c);
//      AddLineStatic(100, "","See http://yacas.sf.net for more information and documentation on Yacas.", font, c);
//      AddLineStatic(100, "","Numeric mode: \""+BigNumber.NumericLibraryName()+"\"\n", font, c);
//      AddLineStatic(100, "","", font, c);

//      AddLineStatic(100, "","Type '?', '??' or 'help' for help, or type '?function' for help on a function.\n", font, c);
      AddLineStatic(100, "","Type 'restart' to restart Yacas, or 'cls' to clear screen.\n", font, c);
      AddLineStatic(100, "","To see example commands, keep typing 'Example();'\n", font, c);

    }



    {
      String docbase = getDocumentBase().toString();
  
//      AddLineStatic(100, ""," '" + docbase + "'.", font, Color.red);

      if (docbase.substring(0,4).equals("file"))
      {
        int pos = docbase.lastIndexOf("/");
        String zipFileName = docbase.substring(0,pos+1)+"yacas.jar";
        if (getParameter("debug") != null)
        {
          AddLineStatic(100, ""," '" + zipFileName + "'.", font, Color.red);
        }
        try
        {
          java.util.zip.ZipFile z = new java.util.zip.ZipFile(new File(new java.net.URI(zipFileName)));
          LispStandard.zipFile = z;
        }
        catch(Exception e)
        {
          out.println("Failed to find yacas.jar");
          out.println(""+zipFileName+" : \n");
          out.println(e.toString());
        }
      }
      if (docbase.startsWith("http"))
      {
        //jar:http://www.xs4all.nl/~apinkus/yacas.jar!/
        int pos = docbase.lastIndexOf("/");
        String scriptBase = "jar:"+ docbase.substring(0,pos+1)+"yacas.jar!/";
        if (getParameter("debug") != null)
        {
          AddLineStatic(100, ""," '" + scriptBase + "'.", font, Color.red);
        }
        yacas.Evaluate("DefaultDirectory(\""+scriptBase+"\");");
      }
    }

    try
    {
      out.println("");
    }
    catch (Exception e)
    {
      out.println(e);
    }

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

    gotDatahubInit = false;
    TryInitThroughDatahub();
  
    i=1;
    while (true)
    {
      String argn = "history"+i;
      String s = getParameter(argn);
      if (s == null) break;
      s = unescape(s);
      AppendHistoryLine(s);
      i++;
    }

    ResetInput();
  }

  boolean gotDatahubInit = false;
  void TryInitThroughDatahub()
  {
    if (!gotDatahubInit)
    {
      String programMode = getParameter("programMode");
      if (programMode == null)
      {
        gotDatahubInit = true;
      }
      else
      {
        try
        {
          Applet dataHub = getAppletContext().getApplet( "datahub");
          if (dataHub != null)
          {
            net.sf.yacas.DatahubApplet cons = (net.sf.yacas.DatahubApplet)dataHub;
            cons.setProgramMode(programMode);
            String programContentsToLoad = "["+cons.getProgram()+"];";
            gotDatahubInit = true; // We're already satisfied here, as we got the contents from the datahub.
            InvokeCalculationSilent(programContentsToLoad);
          }
        }
        catch (Exception e)
        {
        }
      }
    }
  }
  
  public void stop()
  {
  }
  public void AppendHistoryLine(String line)
  {
    //TODO optimize! We need to wrap around the history buffer, this is inefficient.
    if (currentHistoryLine == nrHistoryLines)
    {
      int i;
      for (i=0;i<currentHistoryLine-1;i++)
      {
        history[i] = history[i+1];
      }
      currentHistoryLine--;
    }
    history[currentHistoryLine] = line;
    currentHistoryLine++;
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

  public void ResetInput()
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
    inputDirty = true;

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

              AppendHistoryLine(inputLine);
              AddLinesStatic(48,inputPrompt,inputLine);
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
        boolean handled = false;
        if (hintWindow != null)
        {
          if (hintWindow.iAllowSelection)
          {
            handled = true;
            if (hintWindow.iCurrentPos > 0)
            {
              hintWindow.iCurrentPos--;
              repaint();
            }
          }
        }
      
        if (!handled)
        {
          handled = true;
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
        boolean handled = false;
        if (hintWindow != null)
        {
          if (hintWindow.iAllowSelection)
          {
            handled = true;
            if (hintWindow.iCurrentPos < hintWindow.iNrLines-1)
            {
              hintWindow.iCurrentPos++;
              repaint();
            }
          }
        }
      
        if (!handled)
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
        boolean handled = false;

        if (!handled)
        {
          handled = true;
          if (cursorPos<inputLine.length())
          {
            cursorPos++;
            RefreshHintWindow();
            repaint();
            return;
          }
        }
      }
      else if (e.VK_ENTER == e.getKeyCode())
      {
        boolean handled = false;

        if (!handled)
        {
          if (matchToInsert.length() > 0)
          {
            handled = true;
            inputLine = inputLine.substring(0,ito) + matchToInsert + inputLine.substring(ito,inputLine.length());
            cursorPos += matchToInsert.length();
            RefreshHintWindow();
            repaint();
            return;
          }
        }
        if (!handled)
        {
          if (hintWindow != null)
          {
            if (hintWindow.iAllowSelection)
            {
              handled = true;
              String item = hintWindow.iText[hintWindow.iCurrentPos];
              if (lastMatchedWord.equals(item))
              {
                item = "(";
              }
              else
              {
                item = item.substring(lastMatchedWord.length(),item.length());
              }
              inputLine = inputLine.substring(0,ito) + item + inputLine.substring(ito,inputLine.length());
              cursorPos += item.length();
              RefreshHintWindow();
              repaint();
              return;
            }
          }
        }
        if (!handled)
        {
          if (inputLine.length() > 0)
          {
            AppendHistoryLine(inputLine);
            AddLinesStatic(48,inputPrompt,inputLine);
            if (inputLine.charAt(inputLine.length()-1) == '\\')
              gatheredMultiLine = gatheredMultiLine + inputLine.substring(0,inputLine.length()-1);
            else
              PerformRequest("Out> ",gatheredMultiLine+inputLine);
            ResetInput();
            RefreshHintWindow();
            repaint(0);
          }
        }
      }
      inputDirty=true;
      repaint();
    }
    else if (KeyEvent.KEY_TYPED == e.getID())
    {
      int c = (int)e.getKeyChar();
      if (c>=32 && c < 128)
      {
        inputLine = new StringBuffer(inputLine).insert(cursorPos,e.getKeyChar()).toString();
        cursorPos++;
        RefreshHintWindow();
        inputDirty=true;
        repaint();
      }
    }
  }
  void PerformRequest(String outputPrompt,String inputLine)
  {
    boolean succeed = false;
    if (inputLine.equals("restart"))
    {
      stop();
      start();
      return;
    }
    else if (inputLine.equals("cls"))
    {
      int i;
      for (i=0;i<nrLines;i++) lines[i] = null;
      outputDirty = true;
      succeed = true;
    }
    else if (inputLine.equals("?license") || inputLine.equals("?licence") || inputLine.equals("?warranty"))
    {
      try
      {
        getAppletContext().showDocument( new URL("gpl.html"),"license");
        succeed = true;
      }
      catch (Exception e)
      {
      }
    }
    else
    {
      outp.delete(0,outp.length());
      String response = yacas.Evaluate(inputLine);
      if (outp.length() > 0)
      {
        AddLinesStatic(48,"",outp.toString());
      }
      if (yacas.iError != null)
      {
        AddLinesStatic(48,"Error> ",yacas.iError);
      }
      AddLinesStatic(48, outputPrompt,response);
      succeed = true;
    }
    {
      if (!succeed)
      {
        out.println("Request failed");
      }
    }
  }
  void AddLinesStatic(int indent, String prompt,String str)
  {
    String err = yacas.iError;
    int pos;
    while ((pos = str.indexOf('\n')) >=0)
    {
      AddLineStatic(indent, prompt,str.substring(0,pos),font,Color.black);
      str = str.substring(pos+1,str.length());
    }
    if (str.length()>0) AddLineStatic(indent, prompt,str,font,Color.black);
  }

  public abstract class MOutputLine
  {
    public abstract void draw(Graphics g, int x,int y);
    public abstract int height(Graphics g);
  }
  class StringLine extends MOutputLine
  {
    StringLine(String aText, Font aFont, Color aColor)
    {
      iText = aText;
      iFont = aFont;
      iColor = aColor;
    }
    public void draw(Graphics g, int x,int y)
    {
      g.setColor(iColor);
      g.setFont(iFont);
      FontMetrics fontMetrics = g.getFontMetrics();
      g.drawString(iText, x, y+fontMetrics.getHeight());
    }
    public int height(Graphics g)
    {
      g.setFont(iFont);
      FontMetrics fontMetrics = g.getFontMetrics();
      return fontMetrics.getHeight();
    }
    private String iText;
    private Font   iFont;
    private Color  iColor;
  }


  class PromptedStringLine extends MOutputLine
  {
    PromptedStringLine(int aIndent, String aPrompt, String aText, Font aPromptFont, Font aFont, Color aPromptColor, Color aColor)
    {
      iIndent = aIndent;
      iPrompt = aPrompt;
      iText = aText;
      iPromptFont = aPromptFont;
      iFont = aFont;
      iPromptColor = aPromptColor;
      iColor = aColor;
    }
    public void draw(Graphics g, int x,int y)
    {
      {
        g.setColor(iPromptColor);
        g.setFont(iPromptFont);
        FontMetrics fontMetrics = g.getFontMetrics();
        g.drawString(iPrompt, x, y+fontMetrics.getHeight());
        if (iIndent != 0)
          x+=iIndent;
        else
          x+=fontMetrics.stringWidth(iPrompt);
      }
      {
        g.setColor(iColor);
        g.setFont(iFont);
        FontMetrics fontMetrics = g.getFontMetrics();
        g.drawString(iText, x, y+fontMetrics.getHeight());
      }
    }
    public int height(Graphics g)
    {
      g.setFont(iFont);
      FontMetrics fontMetrics = g.getFontMetrics();
      return fontMetrics.getHeight();
    }
    int iIndent;
    private String iPrompt;
    private String iText;
    private Font   iPromptFont;
    private Font   iFont;
    private Color  iPromptColor;
    private Color  iColor;
  }



  class ImageLine extends MOutputLine
  {
    ImageLine(Image aImage, Applet aApplet)
    {
      iImage = aImage;
      iApplet = aApplet;
    }
    public void draw(Graphics g, int x,int y)
    {
      if (iImage != null)
      {
        Dimension d = iApplet.getSize();
        g.drawImage(iImage,(d.width-iImage.getWidth(iApplet))/2,y,bkColor,iApplet);
      }
    }
    public int height(Graphics g)
    {
      return iImage.getHeight(iApplet);
    }
    Image iImage;
    Applet iApplet;
  }


  final static int nrLines =  60;
  MOutputLine lines[] = new MOutputLine[nrLines];
  int currentLine=0;
  void AddLine(int index, String text)
  {
    AddLineStatic(index, text);
    repaint(0);
  }

  void AddLineStatic(int indent, String text)
  {
    AddLineStatic(indent, "",text,  font, Color.black);
  }
 
  Color iPromptColor = new Color(128,128,128);
  Font iPromptFont = new Font("helvetica", Font.PLAIN, 12);
  void AddLineStatic(int indent, String prompt, String text,  Font aFont, Color aColor)
  {
    lines[currentLine] = new PromptedStringLine(indent, prompt,text,iPromptFont, aFont,iPromptColor, aColor);
    currentLine = (currentLine+1)%nrLines;
    outputDirty = true;
  }

  /// Drawing current view
  Image yacasLogo = null;
  Image offImg = null;
  Graphics offGra = null;
  public void update(Graphics g)
  {
    paint(g);
  }

  public void paint(Graphics g)
  {
    // draw an offScreen drawing
    Dimension dim = getSize();
    if (offGra == null)
    {
      offImg = createImage(dim.width, dim.height);
      offGra = offImg.getGraphics();
    }
 
    // Render image
    paintToBitmap(offGra);

    // put the OffScreen image OnScreen
    g.drawImage(offImg,0,0,null);
    if (hintWindow != null)
    {
      YacasGraphicsContext context = new YacasGraphicsContext(g,0,0);
      context.SetFontSize(1,fontHeight/*12*/);
      int nr_total_lines = 1; //nrLines;
      Dimension d = getSize();
      hintWindow.draw(5,(int)(d.getHeight()-context.FontHeight()-nr_total_lines*context.FontHeight()),context);
    }
  }
 
  Color bkColor = new Color(255,255,255);
  public void paintToBitmap(Graphics g)
  {
    if ( g instanceof Graphics2D )
    {
      Graphics2D g2d = null;
      g2d = (Graphics2D)g;
      g2d.addRenderingHints( new RenderingHints( RenderingHints.KEY_ANTIALIASING ,
                                              RenderingHints.VALUE_ANTIALIAS_ON ));
    }

    FontMetrics metrics = getFontMetrics(font);

    g.setColor(bkColor);
    int yfrom = 0;
 
    g.setFont(font);
    int inHeight = fontHeight;
 
    int yto = getHeight()-1;
    if (!outputDirty)
      yfrom += getHeight()-inHeight;
    if (!inputDirty)
      yto -= inHeight;
 
    g.clearRect(0,yfrom,getWidth(),yto);
    g.setColor(Color.black);

    int i;
    int y=getHeight()-inHeight-g.getFontMetrics().getHeight();
 
    if (outputDirty)
    {
      for (i=0;i<nrLines;i++)
      {
        int index = (currentLine+i)%nrLines;
        if (lines[index] != null)
        {
          y-=lines[index].height(g);
        }
      }
      for (i=0;i<nrLines;i++)
      {
        int index = (currentLine+i)%nrLines;
        if (lines[index] != null)
        {
          if (y+lines[index].height(g)>0)
          {
            lines[index].draw(g,inset,y);
          }
          y+=lines[index].height(g);
        }
      }
    }
    y=getHeight()-g.getFontMetrics().getDescent();//-fontHeight;
    outputDirty = false;
    if (focusGained)
    {
      if (inputDirty)
      {
        if (y+fontHeight>0)
        {
          int promptLength = metrics.stringWidth(inputPrompt);
          g.setColor(Color.red);
          g.setFont(font);

          g.drawString(inputPrompt, inset, y);
          g.drawString(inputLine, inset+promptLength, y);
          int cursorLocation = promptLength;
          for (i=0;i<cursorPos;i++)
          {
            cursorLocation += metrics.charWidth(inputLine.charAt(i));
          }
          y+=g.getFontMetrics().getDescent();
          g.drawLine(inset+cursorLocation,y,inset+cursorLocation,y-fontHeight);
        }
      }
    }
    else
    {
      String toPrint = "Click here to enter an expression";
      int promptLength = metrics.stringWidth(toPrint);
      g.setColor(Color.blue);
      g.setFont(font);

      g.drawString(toPrint, inset, y);
      y+=g.getFontMetrics().getDescent();
    }
    inputDirty=false;
  }
  String inputLine  = new String();
  String gatheredMultiLine = new String();

  int cursorPos = 0;
  final int inset = 5;
 
  final static String inputPrompt = "In> ";
  final static String outputPrompt = "Out> ";

  static final int fontHeight = 14;
  private Font font = new Font("courier", Font.BOLD, fontHeight);

  private static final int nrHistoryLines = 50;
  public String history[] = new String[nrHistoryLines];
  public int currentHistoryLine = 0;
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
        iApplet.AddLineStatic(0,buffer.toString());
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
      String line = file.ReadLine();
      String[] tokens = new String[16];
      int nrTokens = 0;
      while (line != null)
      {
        if (line.substring(0,2).equals("::"))
          break;
        int i=0;
        nrTokens = 0;
        while (i<line.length())
        {
          int start = i;
          while (line.charAt(i) != ':') i++;
          tokens[nrTokens] = line.substring(start,i);
          nrTokens++;
          i++;
        }
        if (nrTokens>3)
        {
          HintItem hi = new HintItem();
          hi.base = tokens[1];
          hi.hint = tokens[2];
          hi.description = tokens[3];
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
    HintWindow hw = new HintWindow(fontsize);
    return hw;
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
    int i,start;
    start = 0;
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
      int baselen = the_hints.hintTexts[i].base.length();
      if (length == baselen)
      {
        if (text.substring(0,baselen).equals(the_hints.hintTexts[i].base))
        {
          if (hints == null)
          {
            hints = CreateHints(12 /*iDefaultFontSize*/);
            hints.iAllowSelection = false;
          }
          AddHintLine(hints, the_hints.hintTexts[i].hint,the_hints.hintTexts[i].description);
        }
      }
    }
    return hints;
  }

  String lastMatchedWord = "";
  String matchToInsert = "";
  int ito=-1;
  void RefreshHintWindow()
  {
    ito = cursorPos;

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

    matchToInsert = "";
    lastMatchedWord = "";
    if (ito>ifrom)
    {
      lastMatchedWord = inputLine.substring(ifrom,ito);
    }

    hintWindow = null;
    if (lastMatchedWord.length() > 0)
    {
//System.out.println("word is "+word);

      int nr = lastMatchedWord.length();
      int maxHintLines = 18;
      String texts[] = new String[maxHintLines+1];
      int nrHintLines = 0;

      int i;
      for (i=0;i<the_hints.nrHintTexts;i++)
      {
        if (nrHintLines == maxHintLines)
          break;

        if (nr <= (the_hints.hintTexts[i].base).length() &&
            lastMatchedWord.equals(the_hints.hintTexts[i].base.substring(0,nr)))
        {
          boolean add = true;
          if (nrHintLines > 0)
          {
            if (texts[nrHintLines-1].equals(the_hints.hintTexts[i].base))
              add = false;
          }
          if (add)
          {
            texts[nrHintLines++] = the_hints.hintTexts[i].base;
          }
          // Exact match, keep this one line
          if (nrHintLines == 1 && ito != cursorPos && lastMatchedWord.equals(the_hints.hintTexts[i].base))
          {
            break;
          }
        }
      }

      if (nrHintLines == maxHintLines)
      {
        texts[nrHintLines++] = "...";
      }
      if (nrHintLines == 1)
      {
        if (lastMatchedWord.length() < texts[0].length())
        {
          matchToInsert = texts[0].substring(lastMatchedWord.length(),texts[0].length());
        }
        hintWindow = TryToHint(texts[0],texts[0].length());
      }
      else if (nrHintLines > 1)
      {
        hintWindow = CreateHints(12);
        hintWindow.iAllowSelection = true;

        for (i=0;i<nrHintLines;i++)
        {
          AddHintLine(hintWindow, texts[i],"");
        }
      }
    }
  }

  public void InvokeCalculation(String expression)
  {
    if (!gotDatahubInit) start();
    AppendHistoryLine(expression);
    AddLinesStatic(48,"In> ",expression);
    PerformRequest("Out> ",expression);
    ResetInput();
    RefreshHintWindow();
    inputDirty = true;
    outputDirty = true;
    repaint(0);
  }

  String lastError;
  public String calculate(String expression)
  {
    if (!gotDatahubInit) start();

    String result = yacas.Evaluate(expression);
    lastError = yacas.iError;
    return result;
  }
  public String getLastError()
  {
    if (lastError != null)
      return lastError;
    else
      return "";
  }

  public void InvokeCalculationSilent(String expression)
  {
    outp.delete(0,outp.length());
    String response = yacas.Evaluate(expression);
    if (outp.length() > 0)
    {
      AddLinesStatic(48,"",outp.toString());
    }
    if (yacas.iError != null)
    {
      AddLinesStatic(48,"Error> ",yacas.iError);
    }
//TODO remove    AddLinesStatic(48, outputPrompt,response);

/*TODO remove
    PerformRequest("Out> ",expression);
*/
    ResetInput();
    RefreshHintWindow();
    inputDirty = true;
    outputDirty = true;
    repaint(0);
  }



}

