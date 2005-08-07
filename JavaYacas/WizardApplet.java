
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
    g.setColor(Color.white);
    g.clearRect(0,0,getWidth(),getHeight());

    String str = "Hello world!!";
    Font font = new Font("courier", Font.BOLD, 14);
    java.awt.geom.Rectangle2D m = g.getFontMetrics().getStringBounds(str,g);
    int x = (int)((d.width-m.getWidth())/2);
    int y = (d.height-18)/2;
    g.setColor(Color.blue);
    g.setFont(font);
    g.drawString(str, x, y);
  }
}
