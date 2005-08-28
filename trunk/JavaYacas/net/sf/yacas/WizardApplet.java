package net.sf.yacas;


/*
The purpose of the wizard applet is to make it easier for beginners to get started with Yacas.
Initially, one is confronted with a command line prompt, and this can be intimidating for a beginner.
The idea is to provide a wizard bar to the left?/right? that allows one to enter expressions without
having to type them in, initially. This could include automatically sending commands to the
console applet (automatically generated examples, static examples, little dialogs that allow you
to enter arguments to specific commands like integrate and differentiate), but also possibly a link
allowing the user to enter his/her own calculation, and submitting that for inclusion. Multi-step
examples would also be nice.

Ideally, this would be supported by some sort of mark-up language, perhaps even Yacas as an engine.

TODO:
x reading scripts from another zip file
- sending strings to the other applet
x maintaining a list of hotspots with links
- responding to clicking on the links
- prettifying the wizard
- optimzing things: it should not search for links linearly!
- Ugly hack: fixed-length max number of links, needs to become a dynamic grownign array
- extending the examples in the wizard
- change the release scripts to also include the wizard
- give feedback as to which of the two applets has focus, so the user knows to click on the console again,
  or pass key events to the console applet, even better
- feedback on which link was pressed.
- Do not show the wizard until the console accepts input
- automatically skip spaces after links(?)
x rendering/layout of a page.

*/


import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.io.*;
import java.net.*;

public class WizardApplet extends Applet implements KeyListener, FocusListener, ClipboardOwner, MouseListener
{
  public void init() 
  {
    setBackground(Color.white);
    setLayout (null);
    addKeyListener(this);
    addFocusListener(this);
    addMouseListener(this);
  }

  static Font titleFont = new Font("courier", Font.BOLD, 18);
  static Font normalFont = new Font("courier", Font.BOLD, 14);
  static Font linkFont = new Font("courier", Font.ITALIC, 14);

  class LinkRect
  {
    int x;
    int y;
    int width;
    int height;
    int word;
  };
  LinkRect links[] = new LinkRect[1024];
  int nrlinks = 0;


  void AddTextSentence(Font font, boolean underlined, Color color, String text, String link)
  {
//System.out.println("Text = "+text);
    while (text.length()>0)
    {
      int j=1;
      while (j<text.length() && text.charAt(j) != ' ' && text.charAt(j) != '\n') j++;
      String word = text.substring(0,j);
      char c = 0;
//System.out.println("  Word = "+word);
      if (j<text.length())
      {
        c = text.charAt(j);
        text = text.substring(j+1,text.length());
      }
      else
        text = "";
      AddWord(font, underlined, color, word, link); 
      if (c == '\n')
        AddWord(null, false, null, null, null);
    }
  }
  void ParsePage(String pageContents)
  {
    Clear();
    while (pageContents.length() > 0)
    {
      int j=0;
      while (j<pageContents.length() && pageContents.charAt(j) != '[') j++;
      AddTextSentence(normalFont, false, Color.black, pageContents.substring(0,j),"");
      pageContents = pageContents.substring(j,pageContents.length());
      if (j<pageContents.length())
      {
        if (pageContents.charAt(j) == '[')
        {
          if (pageContents.startsWith("[title:"))
          {
            int closePos = pageContents.indexOf("]");
            String title = pageContents.substring(7,closePos);
            pageContents = pageContents.substring(closePos+1,pageContents.length());
            AddTextSentence(titleFont, false, Color.black, title,"");
            AddWord(null, false, null, null, null);
          }
          else if (pageContents.startsWith("[link:"))
          {
            pageContents = pageContents.substring(6,pageContents.length());
            int ddotPos = pageContents.indexOf(":");
            int closePos = pageContents.indexOf("]");
            String words = pageContents.substring(0,ddotPos);
            String link = pageContents.substring(ddotPos+1,closePos);
            AddTextSentence(linkFont, true, Color.blue, words,link);

            pageContents = pageContents.substring(closePos+1,pageContents.length());
          }
          else
          {
            int closePos = pageContents.indexOf("]");
            pageContents = pageContents.substring(closePos+1,pageContents.length());
          }
        }
      }
    }
  }
  private static java.util.zip.ZipFile zipFile = null;
  public void LoadFile(String fileName)
  {
    try
    {
      if (zipFile == null)
      {
        String docbase = getDocumentBase().toString();
        {
          int pos = docbase.lastIndexOf("/");
          String zipFileName = docbase.substring(0,pos+1)+"wizard.zip";
          try
          {
            zipFile = new java.util.zip.ZipFile(new File(new java.net.URI(zipFileName)));
System.out.println("Succeeded in finding "+zipFileName);
          }
          catch(Exception e)
          {
            ParsePage(
            "[title:Yacas helper]"+
            "The start page for the Yacas helper could not be found (the zip file "+zipFileName+" was not available).\n"
            );
          }
        }
      }
      if  (zipFile != null)
      {
        java.util.zip.ZipEntry e = zipFile.getEntry(fileName);
        if (e != null)
        {
          java.io.InputStream s = zipFile.getInputStream(e);
          InputStatus status = new InputStatus();
          StdFileInput input = new StdFileInput(s,status);
          ParsePage(input.StartPtr().toString());
        }
      }
      else
      {
        String docbase = getDocumentBase().toString();
        int pos = docbase.lastIndexOf("/");
        String zipFileName = "jar:"+docbase.substring(0,pos+1)+"wizard.zip!/"+fileName;
System.out.println("Trying to find "+zipFileName);
        InputStatus status = new InputStatus();
        JarInputFile input = new JarInputFile(zipFileName, status);
System.out.println("Succeeded in finding "+zipFileName);
        ParsePage(input.StartPtr().toString());
      }
    }
    catch (Exception e)
    {
      ParsePage(
      "[title:Yacas helper]"+
      "The start page for the Yacas helper could not be found.\n"
      );
    }
  }
  public void start()
  {
    repaint();
    Clear();
    LoadFile("start.yml");
  }
  public void stop()
  {
  }
  public void focusGained(FocusEvent evt) 
  {
    repaint();
  }

  public void focusLost(FocusEvent evt) 
  {
  }

  public void lostOwnership(Clipboard clipboard, Transferable contents)
  {
  }
  public void keyPressed(KeyEvent e)
  {
    Applet console = getAppletContext().getApplet( "console");
    if (console != null)
    {
      net.sf.yacas.ConsoleApplet cons = (net.sf.yacas.ConsoleApplet)console;
      cons.keyPressed(e);
    }
    //processKeyEvent(e);
  }
  public void keyTyped(KeyEvent e)
  {
  }
  public void keyReleased(KeyEvent e)
  {
  }
  
	public void update(Graphics g) 
  {
    paint(g);
  }
  
  Image yacasLogo = null;
  Image offImg = null;
  Graphics offGra = null;

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
    g.drawImage(offImg,0,0,null);
  }  

  boolean outputDirty = true;
  void paintToBitmap(Graphics g)
  {
    if ( g instanceof Graphics2D )
    {
      Graphics2D g2d = null;
      g2d = (Graphics2D)g;
      g2d.addRenderingHints( new RenderingHints( RenderingHints.KEY_ANTIALIASING ,
                                              RenderingHints.VALUE_ANTIALIAS_ON ));
    }
    Dimension d = getSize();
    if (outputDirty)
    {
      g.setColor(Color.white);
      g.clearRect(0,0,getWidth(),getHeight());

      nrlinks = 0;

      int i;
      int x = 0;
      int y = 18;
      int maxHeight = 0;
      for (i=0;i<nrWords;i++)
      {
        if (words[i].word == null)
        {
          x=0;
          y+=maxHeight;
        }
        else
        {
          g.setColor(words[i].color);
          g.setFont(words[i].font);
          int pixWidth = g.getFontMetrics().stringWidth(words[i].word);
          if (x+pixWidth>d.width)
          {
            x=0;
            y+=maxHeight;
            maxHeight = 0;
          }
          if (g.getFontMetrics().getHeight() > maxHeight)
            maxHeight = g.getFontMetrics().getHeight();
          g.drawString(words[i].word, x, y);
          int lineWidth = pixWidth;
          if (words[i].underlined)
          {
            if (i<nrWords)
              if (words[i+1].underlined)
                lineWidth+=5;
            g.drawLine(x,y,x+lineWidth,y);
          }
          
          if (words[i].link != null)
          if (words[i].link.length() > 0)
          {
            LinkRect r = new LinkRect();
            r.x = x;
            r.y = y;
            r.width = lineWidth;
            r.height = maxHeight;
            r.word = i;
            links[nrlinks++] = r;
          }
          
          x+=pixWidth+5;
        }
      }
      outputDirty = false;
    }
  }


  class CWizardWord
  {
    public Font font;
    boolean underlined;
    public Color color;
    public String word;
    public String link;
  }
  static int MAX_WORDS = 1024;
  CWizardWord words[] = new CWizardWord[MAX_WORDS];  
  int nrWords = 0;
  int xCur = 0;
  int yCur = 0;
  void Clear()
  {
    int i;
    for (i=0;i<nrWords;i++)
    {
      words[i] = null;
    }
    nrWords = 0;
    xCur = 0;
    yCur = 0;
    outputDirty = true;
  }
  void AddWord(Font font, boolean underlined, Color color, String word, String link)
  {
    CWizardWord theWord = new CWizardWord();
    theWord.font = font;
    theWord.underlined = underlined;
    theWord.color = color;
    theWord.word = word;
    theWord.link = link;
//System.out.println("nrWords = "+nrWords);
    words[nrWords] = theWord;
    nrWords++;
    outputDirty = true;
  }


  // Invoked when the mouse button has been clicked (pressed and released) on a component.
  public void	mouseClicked(MouseEvent e) 
  {
    int x = e.getX();
    int y = e.getY();
//System.out.println("mouseclick on "+x+", "+y);
    int i;
    for (i=0;i<nrlinks;i++)
    {
//System.out.println(""+i+" : ("+links[i].x+", "+links[i].y+") : ("+links[i].width+", "+links[i].height+")"+words[links[i].word].link);
      if (x>=links[i].x && x<=links[i].x+links[i].width)
      {
        if (y<=links[i].y && y>=links[i].y-links[i].height)
        {
          String link = words[links[i].word].link;
System.out.println("link pressed was "+link);
          if (link.endsWith(".yml"))
          {
            LoadFile(words[links[i].word].link);
            repaint();
          }
          else
          {
            Applet console = getAppletContext().getApplet( "console");
            if (console != null)
            {
              net.sf.yacas.ConsoleApplet cons = (net.sf.yacas.ConsoleApplet)console;
              cons.InvokeCalculation(link);
            }
          }
          return;
        }
      }
    }
  }

  // Invoked when the mouse enters a component.
  public void	mouseEntered(MouseEvent e) 
  {
  }
  
  // Invoked when the mouse exits a component.
  public void	mouseExited(MouseEvent e) 
  {
  }
  
  // Invoked when a mouse button has been pressed on a component.
  public void	mousePressed(MouseEvent e) 
  {
  }
  
  // Invoked when a mouse button has been released on a component.
  public void	mouseReleased(MouseEvent e) 
  {
  }

}
