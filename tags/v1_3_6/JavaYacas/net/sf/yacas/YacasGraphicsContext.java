package net.sf.yacas;


import java.awt.*;

class YacasGraphicsContext
{
  Graphics graphics;
  int xtop;
  int ytop;
  public YacasGraphicsContext(Graphics g,int x, int y)
  {
    graphics = g;
    xtop = x;
    ytop = y;
  }

  public void SetColor(int red, int green, int blue)
  {
    graphics.setColor(new Color(red,green,blue));
  }
  public void DrawText(int x, int y, String text)
  {
    graphics.drawString(text,x+xtop,y+ytop);
  }
  public void DrawLine(int x0, int y0, int x1, int y1)
  {
    graphics.drawLine(xtop+x0,ytop+y0,xtop+x1,ytop+y1);
  }
  public void DrawRoundRect(int x,int y, int width, int height, int arc)
  {
    graphics.drawRoundRect(xtop+x,ytop+y,width,height,arc,arc);
  }
  public void DrawRect(int x,int y, int width, int height)
  {
    graphics.drawRect(xtop+x,ytop+y,width,height);
  }
  public void FillRoundRect(int x,int y, int width, int height,int arc)
  {
    graphics.fillRoundRect(xtop+x,ytop+y,width,height,arc,arc);
  }
  public void FillRect(int x,int y, int width, int height)
  {
    graphics.fillRect(xtop+x,ytop+y,width,height);
  }
  public int FontHeight()
  {
    FontMetrics fontMetrics = graphics.getFontMetrics();
    return fontMetrics.getHeight();
  }
  public int FontDescent()
  {
    FontMetrics fontMetrics = graphics.getFontMetrics();
    return fontMetrics.getDescent();
  }
  public int TextWidthInPixels(String text)
  {
    FontMetrics fontMetrics = graphics.getFontMetrics();
    return fontMetrics.stringWidth(text);
  }
  public int SetFontSize(int aBold, int aSize)
  {
    if (aBold != 0)
      graphics.setFont(new Font("Helvetica", Font.BOLD, aSize));
    else
      graphics.setFont(new Font("Helvetica", Font.PLAIN, aSize));
    return 1;
  }
};
