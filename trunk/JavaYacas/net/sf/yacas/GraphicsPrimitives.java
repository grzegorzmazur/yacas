

package net.sf.yacas;

import java.awt.*;

class GraphicsPrimitives
{
  public GraphicsPrimitives(Graphics g)
  {
    iG = g;
    if ( g instanceof Graphics2D )
    {
      iG2D = (Graphics2D)g;
    }
  }
  public void SetLineThickness(float aThickness)
  {
    if (iG2D != null)
    {
      iG2D.setStroke(new BasicStroke((float)(aThickness*viewScale),BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND));
    }
  }

  public void DrawLine(int x0, int y0, int x1, int y1)
  {
    iG.drawLine((int)(x0*viewScale),(int)(y0*viewScale),(int)(x1*viewScale),(int)(y1*viewScale));
  }

  int prevGray = -1;
  void SetGray(int aGray)
  {
    if (prevGray != aGray)
    {
      prevGray = aGray;
      iG.setColor(new Color(aGray, aGray, aGray));
    }
  }

  public void DrawText(String text, int x, int y)
  {
    iG.drawString(text, (int)(x*viewScale), (int)(y*viewScale));
  }

  int prevSetFontSize = -1;
  FontMetrics metrics = null;

  void SetFontSize(int aSize)
  {
    int newFontSize = (int)(viewScale*aSize);
    if (prevSetFontSize != newFontSize)
    {
      prevSetFontSize = newFontSize;
      Font f = new Font ("Verdana", Font.PLAIN, newFontSize);
      if (f != null)
      {
        iG.setFont(f);
        metrics = iG.getFontMetrics();
      }
    }
  }
  int GetFontSize()
  {
    return (int)(prevSetFontSize/viewScale);
  }
  int TextWidth(String s)
  {
    java.awt.geom.Rectangle2D m = metrics.getStringBounds(s,iG);
    return (int)(m.getWidth()/viewScale);
  }
  int GetAscent()
  {
    return (int)(metrics.getAscent()/viewScale);
  }
  double GetDescent()
  {
    return (int)(metrics.getDescent()/viewScale);
  }


  static double viewScale = 1.0;
  private Graphics iG = null;
  private Graphics2D iG2D = null;
}
