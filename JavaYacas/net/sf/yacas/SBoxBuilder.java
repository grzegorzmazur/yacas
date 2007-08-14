
package net.sf.yacas;

import java.awt.*;

class SBoxBuilder
{
  class SBoxSymbolName extends SBox
  {
    SBoxSymbolName(String aSymbol)
    {
      iSymbol = aSymbol;
    }

    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      int height = SBoxBuilder.FontForSize(aSize);
      g.SetFontSize(height);
      
      iSize = aSize;
      iPosition = aPosition;
      iAscent = g.GetAscent();
      iDimension = new Dimension(g.TextWidth(iSymbol),height);
    }

    public void render(GraphicsPrimitives g)
    {
      g.SetFontSize(SBoxBuilder.FontForSize(iSize));
      g.DrawText(iSymbol, iPosition.x, iPosition.y);
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
//iExpressions[i].drawBoundingBox(g);
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
        

        int iExtent = 0;
        
        if (iExpressions[0] instanceof SBoxSum)
        {
          if (iExpressions[1] != null) iExtent = iExtent + iExpressions[1].iAscent;
          if (iExpressions[2] != null) iExtent = iExtent + iExpressions[2].iAscent;

          int fixMaxWidth = dsfix.width;
          if (dlfix.width > fixMaxWidth) fixMaxWidth = dlfix.width;
          if (dexpr.width > fixMaxWidth) fixMaxWidth = dexpr.width;
          iDimension = new Dimension(fixMaxWidth,dexpr.height+iExtent);
        }
        else
        {
          if (iExpressions[1] != null) iExtent = iExtent + iExpressions[1].iAscent/2;
          if (iExpressions[2] != null) iExtent = iExtent + iExpressions[2].iAscent/2;

          int fixMaxWidth = dsfix.width;
          if (dlfix.width > fixMaxWidth) fixMaxWidth = dlfix.width;
          iDimension = new Dimension(dexpr.width+fixMaxWidth,dexpr.height+iExtent);
        }
        iAscent = iExpressions[0].getCalculatedAscent()+iExtent;
      }
      if (aPosition != null)
      {
        Dimension dsfix = new Dimension(0,0);
        Dimension dlfix = new Dimension(0,0);
        Dimension dexpr = iExpressions[0].getDimension();
        if (iExpressions[1] != null) dsfix = iExpressions[1].getDimension();
        if (iExpressions[2] != null) dlfix = iExpressions[2].getDimension();
        iExpressions[0].calculatePositions(g, aSize, new Point(aPosition.x,aPosition.y));

        if (iExpressions[0] instanceof SBoxSum)
        {
          if (iExpressions[1] != null) iExpressions[1].calculatePositions(g, aSize-1, new Point(aPosition.x,aPosition.y-iExpressions[0].iAscent-dsfix.height));
          if (iExpressions[2] != null) iExpressions[2].calculatePositions(g, aSize-1, new Point(aPosition.x,aPosition.y + iExpressions[2].iAscent+dlfix.height));
        }
        else
        {
          if (iExpressions[1] != null) iExpressions[1].calculatePositions(g, aSize-1, new Point(aPosition.x+dexpr.width,aPosition.y-iExpressions[0].iAscent + iExpressions[1].iAscent/2));
          if (iExpressions[2] != null) iExpressions[2].calculatePositions(g, aSize-1, new Point(aPosition.x+dexpr.width,aPosition.y + iExpressions[2].iAscent/2));
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
        iDimension = new Dimension(totalWidth+spacing*(iWidth-1),totalHeight+spacing*(iHeight-1));
        iAscent = iDimension.height/2;
      }
      if (aPosition != null)
      {
        int i,j;
        int w = 0;
        for (i=0;i<iWidth;i++)
        {
          int h=-iAscent;
          for (j=0;j<iHeight;j++)
          {
            iExpressions[i+j*iWidth].calculatePositions(g, aSize, new Point(aPosition.x+w,aPosition.y+h));
            h+=iHeights[j]+spacing;
          }
          w+=iWidths[i]+spacing;
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
    public void calculatePositions(GraphicsPrimitives g, int aSize, java.awt.Point aPosition)
    {
      iSize = aSize;
      iPosition = aPosition;

      if (iDimension == null)
      {
        iExpressions[0].calculatePositions(g,aSize,null);
        iExpressions[1].calculatePositions(g,aSize,null);
        Dimension ndim = iExpressions[0].getDimension();
        Dimension ddim = iExpressions[1].getDimension();
        int width = ndim.width;
        if (width < ddim.width) width = ddim.width;
        iDimension = new Dimension(width,ndim.height + ddim.height + 6);
        iAscent = ndim.height+SBoxBuilder.FontForSize(iSize)/2;
      }
      if (aPosition != null)
      {
        Dimension ndim = iExpressions[0].getDimension();
        Dimension ddim = iExpressions[1].getDimension();
        
        int ynumer = aPosition.y-ndim.height + SBoxBuilder.FontForSize(iSize)/2-2;
        int ydenom = aPosition.y+iExpressions[1].getCalculatedAscent()-SBoxBuilder.FontForSize(iSize)/2+6-2;
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
      g.DrawLine(iPosition.x,iPosition.y+2-SBoxBuilder.FontForSize(iSize)/2,iPosition.x+width,iPosition.y+2-SBoxBuilder.FontForSize(iSize)/2);
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
        iBracketWidth = g.TextWidth(iOpen);
        
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
      g.SetFontSize(iFontSize);
//  System.out.println("iFontSize = "+iFontSize+", iAscent = "+iAscent+", gasc = "+g.GetAscent()+", dimh = "+dim.height);
      int offset = (iFontSize-iAscent)/2;
      g.DrawText(iOpen, iPosition.x, iPosition.y+offset);
      g.DrawText(iClose, iPosition.x+dim.width+iBracketWidth, iPosition.y+offset);

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
      case 0: return 8;
      case 1: return 10;
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


  //TODO remove    Font font = new Font(fontName, Font.BOLD, 36);
  void process(String aType)
  {
    if (aType.equals("="))
    {
      SBox right = pop();
      SBox left = pop();
      push(new SBoxInfixOperator(left,new SBoxSymbolName("="),right));
    }
    else if (aType.equals("/"))
    {
      SBox denom = pop();
      SBox numer = pop();
      push(new SBoxDivisor(numer, denom));        
    }
    else if (aType.equals("+"))
    {
      SBox right = pop();
      SBox left = pop();
      push(new SBoxInfixOperator(left,new SBoxSymbolName("+"),right));
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
    else if (aType.equals(","))
    {
      SBox right = pop();
      SBox left = pop();
      push(new SBoxInfixOperator(left,new SBoxSymbolName(","),right));
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
      if (left instanceof SBoxSubSuperfix)
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
}
