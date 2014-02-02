package net.sf.yacas;



class HintWindow
{
  public  HintWindow(int aTextSize)
  {
    iNrLines = 0;
    iNrDescriptions = 0;
    iMaxWidth = 0;
    iTextSize = aTextSize;
    iCurrentPos = 0;
  }
  public  void AddLine(String aText)
  {
    if (iNrLines >= 20) return;
    iText[iNrLines] = aText;
    iNrLines++;
    iMaxWidth = 0;
  }
  public  void AddDescription(String aText)
  {
    if (iNrDescriptions >= 20) return;
    iDescription[iNrDescriptions] = aText;
    iNrDescriptions++;
    iMaxWidth = 0;
  }
  public  void draw(int x, int y, YacasGraphicsContext  aGraphicsContext)
  {
    aGraphicsContext.SetFontSize(0,iTextSize);
    if (iMaxWidth == 0)
    {
      int i;
      for (i=0;i<iNrLines;i++)
      {
        int width = aGraphicsContext.TextWidthInPixels(iText[i]);
        if (width>iMaxWidth)
            iMaxWidth = width;
      }
      for (i=0;i<iNrDescriptions;i++)
      {
        int width = aGraphicsContext.TextWidthInPixels(iDescription[i]);
        if (width>iMaxWidth)
            iMaxWidth = width;
      }
      iMaxWidth = iMaxWidth + 8;
    }

//System.out.println("iNrLines = "+iNrLines);
//System.out.println("iMaxWidth = "+iMaxWidth);

    int ix = x;
    int iy = y;
    int w = 5+iMaxWidth;
    int h = height(aGraphicsContext);
    iy -= (h+4);

    if (!iAllowSelection)
      aGraphicsContext.SetColor(221,221,238);
    else
      aGraphicsContext.SetColor(221,221,238);
    aGraphicsContext.FillRect(ix,iy,w,h);
    aGraphicsContext.SetColor(0,0,0);
    aGraphicsContext.DrawRect(ix,iy,w,h);

    int i;
 
//System.out.println("iTextSize = "+iTextSize);
//System.out.println("aGraphicsContext.FontHeight() = "+aGraphicsContext.FontHeight());

    for (i=0;i<iNrLines;i++)
    {
      if (!iAllowSelection)
      {
        aGraphicsContext.SetColor(0,0,0);
      }
      else
      {
        if (i == iCurrentPos)
        {
          aGraphicsContext.SetColor(190,190,200);
          aGraphicsContext.FillRect(1+ix,1+iy+(i)*aGraphicsContext.FontHeight(),w-2,aGraphicsContext.FontHeight()-2);
          aGraphicsContext.SetColor(0,0,0);
        }
        else
        {
          aGraphicsContext.SetColor(0,0,0);
        }
      }
        aGraphicsContext.DrawText(ix+2,iy+(i+1)*aGraphicsContext.FontHeight()-aGraphicsContext.FontDescent(),iText[i]);
    }

    if (iNrDescriptions>0)
    {
      int offset = (iNrLines+1)*aGraphicsContext.FontHeight()+7;
 
      aGraphicsContext.DrawLine(ix+6,iy+offset-4-aGraphicsContext.FontHeight(),ix+w-6,iy+offset-4-aGraphicsContext.FontHeight());
 
      aGraphicsContext.SetFontSize(1,iTextSize);
      for (i=0;i<iNrDescriptions;i++)
      {
        aGraphicsContext.DrawText(ix+2,iy+offset+(i)*aGraphicsContext.FontHeight()-aGraphicsContext.FontDescent(),iDescription[i]);
      }
    }
  }



  public  int height(YacasGraphicsContext  aGraphicsContext)
  {
    int h;
    aGraphicsContext.SetFontSize(0,iTextSize);
    h = iNrLines*aGraphicsContext.FontHeight()+2;
 
    if (iNrDescriptions>0)
    {
      aGraphicsContext.SetFontSize(1,iTextSize);
      h += iNrDescriptions*aGraphicsContext.FontHeight()+2;
      // space for line
      h+=7;
    }
    return h;
  }
  public  int iNrLines;


  public  String[] iText = new String[64];
  public  String[] iDescription = new String[64];

  public  boolean iAllowSelection = true;
  public  int iNrDescriptions;
  public  int iMaxWidth;
  public  int iTextSize;
  public  int iCurrentPos;
}



