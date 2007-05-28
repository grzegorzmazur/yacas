package net.sf.yacas;


class SelectSymbol
{

  public   SelectSymbol(int width, int spacing)
  {
    ix = iy = 0;
    nr_symbols = 0;
    iwidth = width;
    ispacing = spacing;
    int i;

    iheight=0;
    AddSymbol("0");
    AddSymbol("1");
    AddSymbol("(");
    AddSymbol(")");
    AddSymbol("[");
    AddSymbol("]");
    AddSymbol("{");
    AddSymbol("}");
    AddSymbol("+");
    AddSymbol("-");
    AddSymbol("*");
    AddSymbol("/");
    AddSymbol(".");
    AddSymbol(",");
    AddSymbol(";");
    AddSymbol(":");
    AddSymbol("!");
    AddSymbol("#");
    AddSymbol("_");
    AddSymbol("^");
    AddSymbol("<");
    AddSymbol(">");
    AddSymbol("<=");
    AddSymbol(">=");
    AddSymbol(":=");
    AddSymbol("=");
    AddSymbol("!");
    AddSymbol("@");
    iheight = (nr_symbols+iwidth-1)/iwidth;
  }
  public   void draw(int xpos, int ypos, YacasGraphicsContext  aGraphicsContext)
  {
    aGraphicsContext.SetFontSize(0,12);
    aGraphicsContext.SetColor(255,255,255);
    aGraphicsContext.FillRect(xpos,ypos,iwidth*ispacing,iheight*aGraphicsContext.FontHeight());
    aGraphicsContext.SetColor(0,0,0);
    aGraphicsContext.DrawRect(xpos,ypos,iwidth*ispacing,iheight*aGraphicsContext.FontHeight());
 
    int x,y;
 
    aGraphicsContext.SetColor(128,128,128);
    for (y=0;y<iheight;y++)
    {
      aGraphicsContext.DrawLine(xpos,                ypos+y*aGraphicsContext.FontHeight(),
                                xpos+iwidth*ispacing,ypos+y*aGraphicsContext.FontHeight());
    }
    for (x=0;x<iwidth;x++)
    {
      aGraphicsContext.DrawLine(xpos+x*ispacing,ypos,
                                xpos+x*ispacing,ypos+iheight*aGraphicsContext.FontHeight());
    }
    aGraphicsContext.SetColor(0,0,0);
    for (y=0;y<iheight;y++)
    {
      for (x=0;x<iwidth;x++)
      {
        if (ix == x && iy == y)
        {
          aGraphicsContext.SetColor(128,128,128);
          aGraphicsContext.FillRect(xpos+x*ispacing,ypos+y*aGraphicsContext.FontHeight(),ispacing,aGraphicsContext.FontHeight());
          aGraphicsContext.SetColor(0,0,0);
        }


        if (symbols[y*iwidth+x] != null)

        {
          aGraphicsContext.DrawText(xpos+x*ispacing+(ispacing-aGraphicsContext.TextWidthInPixels(symbols[y*iwidth+x]))/2,ypos+(y+1)*aGraphicsContext.FontHeight()-aGraphicsContext.FontDescent(),symbols[y*iwidth+x]);
        }
      }
    }
  }


  public   void AddSymbol(String sym)
  {
    symbols[nr_symbols++] = sym;
  }

  public   int ix,iy;
  public   int iwidth,iheight;
  public   int ispacing;
  public   int height(YacasGraphicsContext  aGraphicsContext)
  {
    return iheight*aGraphicsContext.FontHeight();
  }


  String[] symbols = new String[64];


  public   int nr_symbols;
};


