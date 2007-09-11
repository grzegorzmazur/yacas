
package net.sf.yacas;

import java.awt.*;

class SBoxBuilder
{
  class SBoxSymbolName extends SBox
  {
    SBoxSymbolName(String aSymbol)
    {
      iSymbol = aSymbol;
      if (iSymbol.indexOf("\\") == 0)
      {
        if (iSymbol.equals("\\pi")) {}
        else if (iSymbol.equals("\\infty")) {}
        else if (iSymbol.equals("\\cdot")) {}
        else if (iSymbol.equals("\\wedge")) {}
        else if (iSymbol.equals("\\vee")) {}
        else if (iSymbol.equals("\\neq")) {}
        else
        {
          iSymbol = iSymbol.substring(1);
        }
      }
    }

    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      int height = SBoxBuilder.FontForSize(aSize);
      g.SetFontSize(height);

      iSize = aSize;
      iPosition = aPosition;
      if (iSymbol.equals("\\pi") || iSymbol.equals("\\wedge") || iSymbol.equals("\\vee"))
      {
        iDimension = new Dimension(g.TextWidth("M"),height);
        iAscent = g.GetAscent();
      }
      else if (iSymbol.equals("\\neq"))
      {
        iDimension = new Dimension(g.TextWidth("="),height);
        iAscent = g.GetAscent();
      }
      else if (iSymbol.equals("\\infty"))
      {
        iDimension = new Dimension(g.TextWidth("oo"),height);
        iAscent = g.GetAscent();
      }
      else if (iSymbol.equals("\\cdot"))
      {
        iDimension = new Dimension(g.TextWidth("."),height);
        iAscent = g.GetAscent();
      }
      else
      {
        iAscent = g.GetAscent();
        iDimension = new Dimension(g.TextWidth(iSymbol),height);
      }
    }

    public void render(GraphicsPrimitives g)
    {
      if (iSymbol.equals("\\pi"))
      {
        double deltax = 0.15*iDimension.width;
        double deltay = 0.2*iDimension.height;

        g.DrawLine((int)(iPosition.x                 +1*deltax),(int)(iPosition.y-iAscent+2*deltay),(int)(iPosition.x+iDimension.width-1*deltax),(int)(iPosition.y-iAscent                  +2*deltay));
        g.DrawLine((int)(iPosition.x                 +2*deltax),(int)(iPosition.y-iAscent+2*deltay),(int)(iPosition.x                 +2*deltax),(int)(iPosition.y-iAscent+iDimension.height+0*deltay));
        g.DrawLine((int)(iPosition.x+iDimension.width-2*deltax),(int)(iPosition.y-iAscent+2*deltay),(int)(iPosition.x+iDimension.width-2*deltax),(int)(iPosition.y-iAscent+iDimension.height+0*deltay));
      }
      else if (iSymbol.equals("\\wedge") || iSymbol.equals("\\vee") )
      {
        double deltax = 0.15*iDimension.width;
        double deltay = 0.2*iDimension.height;
        int ytip = (int)(iPosition.y-iAscent+iDimension.height+0*deltay);
        int ybase = (int)(iPosition.y-iAscent+2*deltay);
        if (iSymbol.equals("\\wedge"))
        {
          int swap = ytip;
          ytip = ybase;
          ybase = swap;
        }
        g.DrawLine((int)(iPosition.x                 +1*deltax), ybase, iPosition.x + iDimension.width/2, ytip);
        g.DrawLine((int)(iPosition.x+iDimension.width-1*deltax), ybase, iPosition.x + iDimension.width/2, ytip);
      }
      else if (iSymbol.equals("\\neq"))
      {
        g.SetFontSize(SBoxBuilder.FontForSize(iSize));
        g.DrawText("=", iPosition.x, iPosition.y);
        g.DrawLine(iPosition.x+(2*iDimension.width)/3,iPosition.y-iAscent+(2*iDimension.height)/6,
                   iPosition.x+(1*iDimension.width)/3,iPosition.y-iAscent+(6*iDimension.height)/6);
      }
      else if (iSymbol.equals("\\infty"))
      {
        g.SetFontSize(SBoxBuilder.FontForSize(iSize));
        g.DrawText("o", iPosition.x+1, iPosition.y);
        g.DrawText("o", iPosition.x+g.TextWidth("o")-2, iPosition.y);
      }
      else if (iSymbol.equals("\\cdot"))
      {
        int height = SBoxBuilder.FontForSize(iSize);
        g.SetFontSize(height);
        g.DrawText(".", iPosition.x, iPosition.y-height/3);
      }
      else
      {
        g.SetFontSize(SBoxBuilder.FontForSize(iSize));
        g.DrawText(iSymbol, iPosition.x, iPosition.y);
      }
    }
    public String iSymbol;
  }


  abstract class SBoxCompoundExpression extends SBox
  {
    SBoxCompoundExpression(int aNrSubExpressions)
    {
      iExpressions = new SBox[aNrSubExpressions];
    }
    SBox iExpressions[];

    public void render(GraphicsPrimitives g)
    {
//drawBoundingBox(g);
      int i;
      for (i=0;i<iExpressions.length;i++)
      {
        if (iExpressions[i] != null)
        {
          iExpressions[i].render(g);
        }
      }
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
      int i;
      for (i=0;i<iExpressions.length;i++)
      {
        if (iExpressions[i] != null)
          iExpressions[i].drawBoundingBox(g);
      }
    }
  }

  class SBoxSubSuperfix extends SBoxCompoundExpression
  {
    SBoxSubSuperfix(SBox aExpr, SBox aSuperfix, SBox aSubfix)
    {
      super(3);
      iExpressions[0] = aExpr;
      iExpressions[1] = aSuperfix;
      iExpressions[2] = aSubfix;
    }
    void setSuperfix(SBox aExpression)
    {
      iExpressions[1] = aExpression;
    }
    void setSubfix(SBox aExpression)
    {
      iExpressions[2] = aExpression;
    }

    boolean hasSuperfix()
    {
      return (iExpressions[1] != null);
    }

    int iExtent = 0;
    int iSuperOffset = 0;
    int iSubOffset = 0;

    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      iSize = aSize;
      iPosition = aPosition;

      // Get dimensions first
      if (iDimension == null)
      {
        Dimension dsfix = new Dimension(0,0);
        Dimension dlfix = new Dimension(0,0);
        iExpressions[0].calculatePositions(g, aSize, null);
        if (iExpressions[1] != null) iExpressions[1].calculatePositions(g, aSize-1, null);
        if (iExpressions[2] != null) iExpressions[2].calculatePositions(g, aSize-1, null);
        Dimension dexpr = iExpressions[0].getDimension();
        if (iExpressions[1] != null) dsfix = iExpressions[1].getDimension();
        if (iExpressions[2] != null) dlfix = iExpressions[2].getDimension();

        if (iExpressions[0] instanceof SBoxSum || iExpressions[0] instanceof SBoxInt)
        {
          iSuperOffset = 0;
          iSubOffset = 0;
          if (iExpressions[1] != null) iExtent = iExtent + iExpressions[1].iAscent;
          if (iExpressions[2] != null) iExtent = iExtent + iExpressions[2].iAscent;

          int fixMaxWidth = dsfix.width;
          if (dlfix.width > fixMaxWidth) fixMaxWidth = dlfix.width;
          if (dexpr.width > fixMaxWidth) fixMaxWidth = dexpr.width;
          iDimension = new Dimension(fixMaxWidth,dexpr.height+iExtent);
        }
        else
        {
          if (iExpressions[1] != null)
          {
            iSuperOffset = iExpressions[1].getDimension().height-iExpressions[1].iAscent-iExpressions[0].getDimension().height/4;
            iExtent = iExtent + iSuperOffset + iExpressions[1].iAscent;
          }
          if (iExpressions[2] != null)
          {
            iSubOffset = iExpressions[2].iAscent;

            int delta = iSubOffset + (iExpressions[2].getDimension().height-iExpressions[2].iAscent) - (iExpressions[0].getDimension().height-iExpressions[0].iAscent);
            iExtent = iExtent + delta;

          }

          int fixMaxWidth = dsfix.width;
          if (dlfix.width > fixMaxWidth) fixMaxWidth = dlfix.width;
          iDimension = new Dimension(dexpr.width+fixMaxWidth,dexpr.height+iExtent);
        }
        iAscent = iExpressions[0].getCalculatedAscent()+iExtent;

if (iExpressions[2] != null)
{
  iAscent = iAscent - iExpressions[2].getDimension().height;
}
      }
      if (aPosition != null)
      {
        Dimension dsfix = new Dimension(0,0);
        Dimension dlfix = new Dimension(0,0);
        Dimension dexpr = iExpressions[0].getDimension();
        if (iExpressions[1] != null) dsfix = iExpressions[1].getDimension();
        if (iExpressions[2] != null) dlfix = iExpressions[2].getDimension();
        iExpressions[0].calculatePositions(g, aSize, new Point(aPosition.x,aPosition.y));

        if (iExpressions[0] instanceof SBoxSum || iExpressions[0] instanceof SBoxInt)
        {
          if (iExpressions[1] != null) iExpressions[1].calculatePositions(g, aSize-1, new Point(aPosition.x,aPosition.y-iExpressions[0].iAscent-dsfix.height));
          if (iExpressions[2] != null) iExpressions[2].calculatePositions(g, aSize-1, new Point(aPosition.x,aPosition.y + iExpressions[2].iAscent+dlfix.height));
        }
        else
        {
          if (iExpressions[1] != null) iExpressions[1].calculatePositions(g, aSize-1, new Point(aPosition.x+dexpr.width,aPosition.y-iExpressions[0].iAscent - iSuperOffset));
          if (iExpressions[2] != null) iExpressions[2].calculatePositions(g, aSize-1, new Point(aPosition.x+dexpr.width,aPosition.y + iSubOffset));
        }
      }
    }
  }


  class SBoxGrid extends SBoxCompoundExpression
  {
    SBoxGrid(int aWidth, int aHeight)
    {
      super(aWidth*aHeight);
      iWidth = aWidth;
      iHeight = aHeight;
    }
    void SetSBox(int x, int y, SBox aExpression)
    {
      iExpressions[x+iWidth*y] = aExpression;
    }
    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      int spacing = 12;

      iSize = aSize;
      iPosition = aPosition;

      // Get dimensions first
      if (iDimension == null)
      {
        int i,j;
        for (i=0;i<iWidth*iHeight;i++)
        {
          iExpressions[i].calculatePositions(g, aSize, null);
        }
        iWidths = new int[iWidth];
        iHeights = new int[iHeight];
        for (i=0;i<iWidth;i++)
          iWidths[i] = 0;
        for (i=0;i<iHeight;i++)
          iHeights[i] = 0;

        for (i=0;i<iWidth;i++)
        {
          for (j=0;j<iHeight;j++)
          {
            Dimension d = iExpressions[i+iWidth*j].getDimension();

            if (iWidths[i] < d.width) iWidths[i] = d.width;
            if (iHeights[j] < d.height) iHeights[j] = d.height;
          }
        }
        int totalWidth = 0;
        for (i=0;i<iWidth;i++)
        {
          totalWidth = totalWidth+iWidths[i];
        }

        int totalHeight = 0;
        for (j=0;j<iHeight;j++)
        {
          totalHeight = totalHeight+iHeights[j];
        }
        iDimension = new Dimension(totalWidth+spacing*(iWidth),totalHeight+spacing*(iHeight));
        iAscent = iDimension.height/2;
      }
      if (aPosition != null)
      {
        int i,j;
        int h = -iAscent;
        for (j=0;j<iHeight;j++)
        {
          int maxAscent = -10000;
          for (i=0;i<iWidth;i++)
          {
            if (maxAscent < iExpressions[i+j*iWidth].iAscent)
              maxAscent = iExpressions[i+j*iWidth].iAscent;
          }
          h = h + maxAscent;
          int w = 0;
          for (i=0;i<iWidth;i++)
          {
            iExpressions[i+j*iWidth].calculatePositions(g, aSize, new Point(aPosition.x+w,aPosition.y+h));
            w+=iWidths[i]+spacing;
          }
          h = h - maxAscent;
          h = h + iHeights[j]+spacing;
        }
      }
    }
    int iWidth;
    int iHeight;
    int iWidths[];
    int iHeights[];
  }

  class SBoxPrefixOperator extends SBoxCompoundExpression
  {
    SBoxPrefixOperator(SBox aLeft, SBox aRight)
    {
      super(2);
      iExpressions[0] = aLeft;
      iExpressions[1] = aRight;
    }
    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      iSize = aSize;
      iPosition = aPosition;

      // Get dimensions first
      if (iDimension == null)
      {
        iExpressions[0].calculatePositions(g,aSize,null);
        iExpressions[1].calculatePositions(g,aSize,null);

        Dimension dleft = iExpressions[0].getDimension();
        Dimension dright = iExpressions[1].getDimension();
        int height = dleft.height;
        if (height<dright.height) height=dright.height;
        iDimension = new Dimension(dleft.width+dright.width+2,height);
        iAscent = iExpressions[0].getCalculatedAscent();
        if (iAscent < iExpressions[1].getCalculatedAscent()) iAscent = iExpressions[1].getCalculatedAscent();
      }
      if (aPosition != null)
      {
        Dimension dleft = iExpressions[0].getDimension();
        Dimension dright = iExpressions[1].getDimension();
        iExpressions[0].calculatePositions(g,aSize,new Point(aPosition.x,aPosition.y/*+(iAscent-iExpressions[0].getCalculatedAscent())*/));
        iExpressions[1].calculatePositions(g,aSize,new Point(aPosition.x+dleft.width+2,aPosition.y/*+(iAscent-iExpressions[1].getCalculatedAscent())*/));
      }
    }
  }

  class SBoxInfixOperator extends SBoxCompoundExpression
  {
    SBoxInfixOperator(SBox aLeft, SBox aInfix, SBox aRight)
    {
      super(3);
      iExpressions[0] = aLeft;
      iExpressions[1] = aInfix;
      iExpressions[2] = aRight;
    }
    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      iSize = aSize;
      iPosition = aPosition;

      // Get dimensions first
      if (iDimension == null)
      {
        iExpressions[0].calculatePositions(g,aSize,null);
        iExpressions[1].calculatePositions(g,aSize,null);
        iExpressions[2].calculatePositions(g,aSize,null);

        Dimension dleft = iExpressions[0].getDimension();
        Dimension dinfix = iExpressions[1].getDimension();
        Dimension dright = iExpressions[2].getDimension();
        int height = dleft.height;
        if (height<dinfix.height) height=dinfix.height;
        if (height<dright.height) height=dright.height;
        iDimension = new Dimension(dleft.width+dinfix.width+dright.width+4,height);
        iAscent = iExpressions[0].getCalculatedAscent();
        if (iAscent < iExpressions[1].getCalculatedAscent()) iAscent = iExpressions[1].getCalculatedAscent();
        if (iAscent < iExpressions[2].getCalculatedAscent()) iAscent = iExpressions[2].getCalculatedAscent();
      }
      if (aPosition != null)
      {
        Dimension dleft = iExpressions[0].getDimension();
        Dimension dinfix = iExpressions[1].getDimension();
        Dimension dright = iExpressions[2].getDimension();
        iExpressions[0].calculatePositions(g,aSize,new Point(aPosition.x,aPosition.y));
        iExpressions[1].calculatePositions(g,aSize,new Point(aPosition.x+dleft.width+2,aPosition.y));
        iExpressions[2].calculatePositions(g,aSize,new Point(aPosition.x+dleft.width+dinfix.width+4,aPosition.y ));
      }
    }
  }


  class SBoxDivisor extends SBoxCompoundExpression
  {
    SBoxDivisor(SBox aNumerator, SBox aDenominator)
    {
      super(2);
      iExpressions[0] = aNumerator;
      iExpressions[1] = aDenominator;
    }
    int iDashheight = 0;
    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      iSize = aSize;
      iPosition = aPosition;

      iDashheight = SBoxBuilder.FontForSize(iSize);

      if (iDimension == null)
      {
        iExpressions[0].calculatePositions(g,aSize,null);
        iExpressions[1].calculatePositions(g,aSize,null);
        Dimension ndim = iExpressions[0].getDimension();
        Dimension ddim = iExpressions[1].getDimension();
        int width = ndim.width;
        if (width < ddim.width) width = ddim.width;
        iDimension = new Dimension(width,ndim.height + ddim.height + iDashheight);
        iAscent = ndim.height+iDashheight;
      }
      if (aPosition != null)
      {
        Dimension ndim = iExpressions[0].getDimension();
        Dimension ddim = iExpressions[1].getDimension();

        int ynumer = aPosition.y-ndim.height +iExpressions[0].getCalculatedAscent()-iDashheight;
        int ydenom = aPosition.y+iExpressions[1].getCalculatedAscent();
        iExpressions[0].calculatePositions(g,aSize,new java.awt.Point(aPosition.x + (iDimension.width-ndim.width)/2,ynumer));
        iExpressions[1].calculatePositions(g,aSize,new java.awt.Point(aPosition.x + (iDimension.width-ddim.width)/2,ydenom));
      }
    }

    public void render(GraphicsPrimitives g)
    {
      super.render(g);

      java.awt.Dimension ndim = iExpressions[0].getDimension();
      java.awt.Dimension ddim = iExpressions[1].getDimension();
      int width = ndim.width;
      if (width < ddim.width) width = ddim.width;

      g.SetLineThickness(1);
      g.DrawLine(iPosition.x,iPosition.y-iDashheight/2+2,iPosition.x+width,iPosition.y-iDashheight/2+2);
    }
  }


  class SBoxSum extends SBox
  {
    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      int height = SBoxBuilder.FontForSize(aSize);
      g.SetFontSize(height);
      iSize = aSize;
      iPosition = aPosition;
      iAscent = height/2+g.GetAscent();
      iDimension = new Dimension((4*height)/3,2*height);
    }

    public void render(GraphicsPrimitives g)
    {
      int height = SBoxBuilder.FontForSize(iSize);
      g.SetLineThickness(2);
      int x0 = iPosition.x;
      int y0 = iPosition.y-iAscent;
      int x1 = x0+iDimension.width;
      int y1 = y0+iDimension.height;
      g.DrawLine(x1,y0,x0,y0);
      g.DrawLine(x0,y0,x0+(2*height)/4,(int)(y0+y1)/2);
      g.DrawLine(x0+(2*height)/4,(int)(y0+y1)/2,x0,y1);
      g.DrawLine(x0,y1,x1,y1);
    }
  }

  class SBoxInt extends SBox
  {
    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      int height = SBoxBuilder.FontForSize(aSize);
      g.SetFontSize(height);
      iSize = aSize;
      iPosition = aPosition;
      iAscent = height/2+g.GetAscent();
      iDimension = new Dimension((1*height)/2,2*height);
    }

    public void render(GraphicsPrimitives g)
    {
      int height = SBoxBuilder.FontForSize(iSize);
      g.SetLineThickness(2);
      int x0 = iPosition.x;
      int y0 = iPosition.y-iAscent;
      int x1 = x0+iDimension.width;
      int y1 = y0+iDimension.height;

      g.DrawLine(x1,y0,x1-iDimension.width/4,y0);
      g.DrawLine(x1-iDimension.width/4,y0,x1-(2*iDimension.width)/4,y0+iDimension.width/4);

      g.DrawLine(x1-(2*iDimension.width)/4,y0+iDimension.width/4, x1-(2*iDimension.width)/4,y0+iDimension.height-iDimension.width/4);

      g.DrawLine(x1-(2*iDimension.width)/4,y0+iDimension.height-iDimension.width/4,x1-(3*iDimension.width)/4,y0+iDimension.height);
      g.DrawLine(x1-(3*iDimension.width)/4,y0+iDimension.height,x0,y0+iDimension.height);


    }
  }



  class SBoxSquareRoot extends SBoxCompoundExpression
  {
    SBoxSquareRoot(SBox aExpression)
    {
      super(1);
      iExpressions[0] = aExpression;
    }
    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      iSize = aSize;
      iPosition = aPosition;

      if (iDimension == null)
      {
        iExpressions[0].calculatePositions(g,aSize,null);
        Dimension dim = iExpressions[0].getDimension();
        iDimension = new Dimension((int)(dim.width+6),dim.height+3);
        iAscent = iExpressions[0].getCalculatedAscent()+3;
      }
      if (aPosition != null)
      {
        Dimension dim = iExpressions[0].getDimension();
        iExpressions[0].calculatePositions(g,aSize,new java.awt.Point((int)(aPosition.x + 6),aPosition.y));
      }
    }
    public void render(GraphicsPrimitives g)
    {
      super.render(g);
      g.SetLineThickness(1);
      Dimension dim = iExpressions[0].getDimension();
      int x0 = iPosition.x;
      int y0 = iPosition.y-iAscent;
      int x1 = x0+dim.width+6;
      int y1 = y0+dim.height+6;
      g.DrawLine(x0,y0+1,x0+3,y1-1);
      g.DrawLine(x0+3,y1-1,x0+6,y0+2);
      g.DrawLine(x0+6,y0+1,x1,y0+1);
    }
  }

  class SBoxBracket extends SBoxCompoundExpression
  {
    SBoxBracket(SBox aExpression, String aOpen, String aClose)
    {
      super(1);
      iOpen = aOpen;
      iClose = aClose;
      iExpressions[0] = aExpression;
    }
    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      iSize = aSize;
      iPosition = aPosition;

      if (iDimension == null)
      {
        iExpressions[0].calculatePositions(g,aSize,null);
        Dimension dim = iExpressions[0].getDimension();
        iFontSize = dim.height;
        g.SetFontSize(dim.height);
        iBracketWidth = SBoxBuilder.FontForSize(aSize)/2;
        iDimension = new Dimension(dim.width+2*iBracketWidth,dim.height);
        iAscent = iExpressions[0].getCalculatedAscent();
      }
      if (aPosition != null)
      {
        Dimension dim = iExpressions[0].getDimension();
        iExpressions[0].calculatePositions(g,aSize,new java.awt.Point(aPosition.x+iBracketWidth,aPosition.y));
      }
    }
    public void render(GraphicsPrimitives g)
    {
      super.render(g);
      Dimension dim = iExpressions[0].getDimension();
      drawBracket(g, iOpen , iPosition.x                        , iPosition.y-getCalculatedAscent() );
      drawBracket(g, iClose, iPosition.x+dim.width+iBracketWidth, iPosition.y-getCalculatedAscent() );
    }

    void drawBracket(GraphicsPrimitives g, String bracket, int x, int y)
    {
      Dimension dim = iExpressions[0].getDimension();
      if (bracket.equals("[") || bracket.equals("]"))
      {
        int margin = 2;
        g.SetLineThickness(2);
        if (bracket.equals("["))
        {
          g.DrawLine(x+margin,y,x+margin,y+dim.height);
        }
        else
        {
          g.DrawLine(x+iBracketWidth-margin,y,x+iBracketWidth-margin,y+dim.height);
        }
        g.SetLineThickness(1);
        g.DrawLine(x+iBracketWidth-margin,y,x+margin,y);
        g.DrawLine(x+margin,y+dim.height,x+iBracketWidth-margin,y+dim.height);
      }
      else if (bracket.equals("(") || bracket.equals(")"))
      {
        int xstart;
        int xend;
        if (bracket.equals("("))
        {
          xstart = x+iBracketWidth;
          xend = x;
        }
        else
        {
          xstart = x;
          xend = x+iBracketWidth;
        }
        int delta = xend-xstart;
        float steps[] = new float[3];
        steps[0] = 0.2f;
        steps[1] = 0.6f;
        steps[2] = 0.8f;
        g.SetLineThickness(1f);
        g.DrawLine((int)(xstart+(delta*steps[0])),y+(0*dim.height)/6,(int)(xstart+(delta*steps[1])),y+(1*dim.height)/6);
        g.SetLineThickness(1.3f);
        g.DrawLine((int)(xstart+(delta*steps[1])),y+(1*dim.height)/6,(int)(xstart+(delta*steps[2])),y+(2*dim.height)/6);
        g.SetLineThickness(1.6f);
        g.DrawLine((int)(xstart+(delta*steps[2])),y+(2*dim.height)/6,(int)(xstart+(delta*steps[2])),y+(4*dim.height)/6);
        g.SetLineThickness(1.3f);
        g.DrawLine((int)(xstart+(delta*steps[2])),y+(4*dim.height)/6,(int)(xstart+(delta*steps[1])),y+(5*dim.height)/6);
        g.SetLineThickness(1f);
        g.DrawLine((int)(xstart+(delta*steps[1])),y+(5*dim.height)/6,(int)(xstart+(delta*steps[0])),y+(6*dim.height)/6);
      }
      else
      {
        g.SetFontSize(iFontSize);
        int offset = (iFontSize-iAscent)/2;
        g.DrawText(bracket, x, y+offset);
      }
    }

    int iFontSize;
    int iBracketWidth;
    String iOpen;
    String iClose;
  }




  static int FontForSize(int aSize)
  {
    if (aSize > 3) aSize = 3;
    if (aSize < 0) aSize = 0;

    switch (aSize)
    {
      case 0: return 6;
      case 1: return 8;
      case 2: return 12;
      case 3: return 16;
      default: return 16;
    }
  }


  SBox stack[] = new SBox[1024];
  int stackDepth = 0;
  public SBox pop()
  {
    stackDepth--;
    SBox result = stack[stackDepth];
    return result;
  }
  void push(SBox aSbox)
  {
    stack[stackDepth] = aSbox;
    stackDepth++;
  }
  public int StackDepth()
  {
    return stackDepth;
  }

  void process(String aType)
  {
    if (aType.equals("=") || aType.equals("\\neq") || aType.equals("+") || aType.equals(",") ||
        aType.equals("\\wedge") || aType.equals("\\vee") ||
        aType.equals("<") || aType.equals(">") ||
        aType.equals("<=") || aType.equals(">=")
        )
    {
      SBox right = pop();
      SBox left = pop();
      push(new SBoxInfixOperator(left,new SBoxSymbolName(aType),right));
    }
    else if (aType.equals("/"))
    {
      SBox denom = pop();
      SBox numer = pop();
      push(new SBoxDivisor(numer, denom));
    }
    else if (aType.equals("-/2"))
    {
      SBox right = pop();
      SBox left = pop();
      push(new SBoxInfixOperator(left,new SBoxSymbolName("-"),right));
    }
    else if (aType.equals("-/1"))
    {
      SBox right = pop();
      push(new SBoxPrefixOperator(new SBoxSymbolName("-"),right));
    }
    else if (aType.equals("~"))
    {
      SBox right = pop();
      push(new SBoxPrefixOperator(new SBoxSymbolName("~"),right));
    }
    else if (aType.equals("!"))
    {
      SBox left = pop();
      push(new SBoxPrefixOperator(left,new SBoxSymbolName("!")));
    }
    else if (aType.equals("*"))
    {
      SBox right = pop();
      SBox left = pop();
      push(new SBoxInfixOperator(left,new SBoxSymbolName(""),right));
    }
    else if (aType.equals("[func]"))
    {
      SBox right = pop();
      SBox left = pop();
      push(new SBoxPrefixOperator(left,right));
    }

    else if (aType.equals("^"))
    {
      SBox right = pop();
      SBox left = pop();
      boolean appendToExisting = false;

      if (left instanceof SBoxSubSuperfix)
      {
        SBoxSubSuperfix sbox = (SBoxSubSuperfix)left;
        if (!sbox.hasSuperfix())
          appendToExisting = true;
      }
      if (appendToExisting)
      {
        SBoxSubSuperfix sbox = (SBoxSubSuperfix)left;
        sbox.setSuperfix(right);
        push(sbox);
      }
      else
      {
        push(new SBoxSubSuperfix(left,right,null));
      }
    }
    else if (aType.equals("_"))
    {
      SBox right = pop();
      SBox left = pop();
      if (left instanceof SBoxSubSuperfix)
      {
        SBoxSubSuperfix sbox = (SBoxSubSuperfix)left;
        sbox.setSubfix(right);
        push(sbox);
      }
      else
      {
        push(new SBoxSubSuperfix(left,null,right));
      }
    }
    else if (aType.equals("[sqrt]"))
    {
      SBox left = pop();
      push(new SBoxSquareRoot(left));
    }
    else if (aType.equals("[sum]"))
    {
      push(new SBoxSum());
    }
    else if (aType.equals("[int]"))
    {
      push(new SBoxInt());
    }
    else if (aType.equals("[roundBracket]"))
    {
      SBox left = pop();
      push(new SBoxBracket(left,"(",")"));
    }
    else if (aType.equals("[squareBracket]"))
    {
      SBox left = pop();
      push(new SBoxBracket(left,"[","]"));
    }
    else if (aType.equals("[accoBracket]"))
    {
      SBox left = pop();
      push(new SBoxBracket(left,"{","}"));
    }
    else if (aType.equals("[grid]"))
    {
      SBox widthBox = pop();
      SBox heightBox = pop();
      int width = Integer.parseInt(((SBoxSymbolName)widthBox).iSymbol);
      int height = Integer.parseInt(((SBoxSymbolName)heightBox).iSymbol);

      SBoxGrid grid = new SBoxGrid(width,height);

      int i,j;
      for (j=height-1;j>=0;j--)
      {
        for (i=width-1;i>=0;i--)
        {
          SBox value = pop();
          grid.SetSBox(i, j, value);
        }
      }
      push(grid);
    }
    else
    {
      push(new SBoxSymbolName(aType));
    }
  }

  public void processLiteral(String aExpression)
  {
    push(new SBoxSymbolName(aExpression));
  }
}
