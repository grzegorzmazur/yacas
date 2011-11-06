
package net.sf.yacas;

import java.awt.*;
import java.applet.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;

public class FormulaViewApplet extends Applet
{
  public void init()
  {
    setBackground(Color.white);
    setLayout(null);
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


  Image offImage = null;
  Graphics offGraphics = null;
  Dimension offDimension = null;
  public void update(Graphics g)
  {
    Dimension d = getSize();
    // Create the offscreen graphics context
    if ((offGraphics == null)
    || (d.width != offDimension.width)
    || (d.height != offDimension.height))
    {
      offDimension = d;
      offImage = createImage(d.width, d.height);
      offGraphics = offImage.getGraphics();
      // Paint the frame into the image
      paintFrame(offGraphics);
    }

    // Paint the image onto the screen
    g.drawImage(offImage, 0, 0, null);
  }

  /**
    * Paint the previous frame (if any).
    */
  public void paint(Graphics g)
  {
    //System.out.println("paint");
    Dimension d = getSize();
    if ((offGraphics == null)
    || (d.width != offDimension.width)
    || (d.height != offDimension.height))
    {
      offDimension = d;
      offImage = createImage(d.width, d.height);
      offGraphics = offImage.getGraphics();

      // Paint the frame into the image
      paintFrame(offGraphics);
    }
    if (offImage != null)
    {
      g.drawImage(offImage, 0, 0, null);
    }
  }
  void paintFrame(Graphics g)
  {
    //System.out.println("paintFrame");
    // Tell the rendering system we'd like to have anti-aliasing please
    if (g instanceof Graphics2D)
    {
      Graphics2D g2d = null;
      g2d = (Graphics2D)g;
      g2d.addRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                                 RenderingHints.VALUE_ANTIALIAS_ON));
    }

    // Clear Background
    Dimension d = getSize();
//    g.setColor(Color.white);
 //   g.fillRect(0, 0, d.width, d.height);

    // All graphics should be black from now on
    g.setColor(Color.black);

    GraphicsPrimitives gp = new GraphicsPrimitives(g);

    gp.SetLineThickness(0);

    if (expression == null)
    {
      String s = getParameter("expression");
      if (s != null)
      {
System.out.println("re-rendering the whole formula!");
        TeXParser parser = new TeXParser();
        expression = parser.parse(s);
      }
    }
    if (expression != null)
    {
      expression.calculatePositions(gp, 3, new java.awt.Point(1, d.height/2));
      expression.render(gp);
    }
  }
  SBox expression = null;
}


