
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
- reading scripts from another zip file
- sending strings to the other applet
- rendering/layout of a page.

Wizard'Main():=
[
  Wizard'Clear();
  Wizard'Title({"Yacas Wizard"});
  Wizard'Text({"This wizard provides various examples accessible from the links below."});
  Wizard'List(
  {
    {Wizard'Link("About","Wizard'About")," this wizard."},
    {"Some simple ",Wizard'Link("arithmetic","Wizard'Arithmetic")," examples"},
    {"Some ",Wizard'Link("calculus","Wizard'Calculus")," examples"},
    {"Some ",Wizard'Link("multi-step examples","Wizard'MultiStep")," examples"},
    {"Edit dialogs for specific ",Wizard'Link("commands","Wizard'Commands")}
  });
];
Wizard'Main();

Wizard'Arithmetic():=
[
  Wizard'Clear();
  Wizard'Title({"Simple arithmetic"});
  Wizard'Text({"Some simple arithmetic."});
  Wizard'List(
  {
    {Wizard'Link("Random addition","Wizard'RandomAddition")," example"}
  });
];

Wizard'RandomAddition():=
[
  Canvas'SendCommand("1+1");
];


[title:Yacas Wizard]
This wizard provides various examples accessible from the links below.
[link:Aboud:about.sml] this wizard.

Some simple [link:arithmetic:arith.sml] examples

Some [link:calculus:calculus.sml] examples

Some [link:multi-step:multistep.sml] examples

Edit dialogs for specific [link:commands:commands.sml]



Canvas'SetFont(name,type,size);
Canvas'SetColor(r,g,b);
Canvas'AddText(text,link);
Canvas'Break();
Canvas'Bullet();
Canvas'SendCommand(command);

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

public class WizardApplet extends Applet implements KeyListener, FocusListener, ClipboardOwner
{
  public void init() 
  {
    setBackground(Color.white);
    setLayout (null);
    addKeyListener(this);
    addFocusListener(this);
  }

  public void start()
  {
    repaint();
    Clear();
    Font font = new Font("courier", Font.BOLD, 14);
    Color color = Color.black;
   

    int i;
    for (i=0;i<10;i++) 
    {
      AddWord(font, color, "Hello", "");
      AddWord(font, color, "world!", "");
    }

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

      int i;
      int x = 0;
      int y = 18;
      for (i=0;i<nrWords;i++)
      {
        g.setColor(words[i].color);
        g.setFont(words[i].font);
        int pixWidth = g.getFontMetrics().stringWidth(words[i].word);
        if (x+pixWidth>d.width)
        {
          x=0;
          y+=g.getFontMetrics().getHeight();
        }
        g.drawString(words[i].word, x, y);
        x+=pixWidth+5;
        
      }

      outputDirty = false;
    }

    String str = "Test text!!";
    Font font = new Font("courier", Font.BOLD, 14);
    java.awt.geom.Rectangle2D m = g.getFontMetrics().getStringBounds(str,g);
    int x = (int)((d.width-m.getWidth())/2);
    int y = (d.height-18)/2;
    g.setColor(Color.blue);
    g.setFont(font);
    g.drawString(str, x, y);

  }


  class CWizardWord
  {
    public Font font;
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
  void AddWord(Font font, Color color, String word, String link)
  {
    CWizardWord theWord = new CWizardWord();
    theWord.font = font;
    theWord.color = color;
    theWord.word = word;
    theWord.link = link;
System.out.println("nrWords = "+nrWords);
    words[nrWords] = theWord;
    nrWords++;
    outputDirty = true;
  }

}
