
package net.sf.yacas;

abstract class SBox
{
  abstract public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition);
  abstract public void render(GraphicsPrimitives g);

  public java.awt.Dimension getDimension()
  {
    return iDimension;
  }
  public java.awt.Point getCalculatedPosition()
  {
    return iPosition;
  }
  public int getSetSize()
  {
    return iSize;
  }
  public int getCalculatedAscent()
  {
    return iAscent;
  }

  public void drawBoundingBox(GraphicsPrimitives g)
  {
    g.SetLineThickness(0);
    int x0 = iPosition.x;
    int y0 = iPosition.y-getCalculatedAscent();
    int x1 = x0+iDimension.width;
    int y1 = y0+iDimension.height;
    g.DrawLine(x0,y0,x1,y0);
    g.DrawLine(x1,y0,x1,y1);
    g.DrawLine(x1,y1,x0,y1);
    g.DrawLine(x0,y1,x0,y0);
  }

  java.awt.Dimension iDimension;
  java.awt.Point iPosition;
  int iSize;
  int iAscent;
}
