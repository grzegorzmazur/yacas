
package net.sf.yacas;

import java.net.*;
import java.awt.*;
import java.io.*;
import java.awt.event.*;

public class GrapherApplet extends java.applet.Applet implements KeyListener
{
  Dimension offDimension;
  Image offImage;
  Graphics offGraphics;

  Grapher grapher;

  String iRenderOperations;

  public void init() 
  {
    iRenderOperations = getParameter("CallList");
    if (iRenderOperations == null)
      iRenderOperations = "";
    grapher = new Grapher(iRenderOperations);
    addKeyListener(this);
    repaint();
  }

  public void SetupCallList(String aCallList)
  {
    if (grapher != null)
    {
      grapher.SetupCallList(aCallList);
      offGraphics = null;
      repaint();
    }
  }

  public void	keyReleased(KeyEvent e) {}
  public void	keyTyped(KeyEvent e) {}
  public void keyPressed(KeyEvent e)
  {
    double scf = 1.05;
    switch (e.getKeyChar())
    {
      case 'o':
      case 'O':
        grapher.xmin *= scf;
        grapher.xmax *= scf;
        offImage = null; offGraphics = null; repaint();
        break;
      case 'p':
      case 'P':
        grapher.xmin /= scf;
        grapher.xmax /= scf;
        offImage = null; offGraphics = null; repaint();
        break;

      case 'a':
      case 'A':
        grapher.ymin *= scf;
        grapher.ymax *= scf;
        offImage = null; offGraphics = null; repaint();
        break;
      case 'z':
      case 'Z':
        grapher.ymin /= scf;
        grapher.ymax /= scf;
        offImage = null; offGraphics = null; repaint();
        break;
    }
  }


  public void start() 
  {
    repaint();
  }

  public void stop() 
  {
    offImage = null;
    offGraphics = null;
  }

  void drawToOffscreen()
  {
    // Create the offscreen graphics context
    Dimension d = getSize();
    if ((offGraphics == null)
      || (d.width != offDimension.width)
      || (d.height != offDimension.height))
    {
      offDimension = d;
      offImage = createImage(d.width, d.height);
      offGraphics = offImage.getGraphics();
      paintFrame(offGraphics);
    }
  }
  public void update(Graphics g) 
  {
    drawToOffscreen();

    // Paint the frame into the image
    synchronized(offImage)
    {
      // Paint the image onto the screen
      g.drawImage(offImage, 0, 0, null);
    }
  }

  public void paint(Graphics g) 
  {
    drawToOffscreen();
    if (offImage != null) 
    {
      synchronized(offImage)
      {
        g.drawImage(offImage, 0, 0, null);
      }
    }
  }

  synchronized public void paintFrame(Graphics g) 
  {
    Dimension d = getSize();
    grapher.paint(g, 0, 0, d);
  }
};

