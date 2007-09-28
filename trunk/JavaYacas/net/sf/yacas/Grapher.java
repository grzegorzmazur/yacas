

package net.sf.yacas;

import java.awt.*;

public class Grapher
{
  Grapher(String aCallList)
  {
    SetupCallList(aCallList);
  }

  public void SetupCallList(String aCallList)
  {
    xmin = 1e200;
    ymin = 1e200;
    xmax = -xmin;
    ymax = -ymin;
    iCallList = aCallList;
    RunCallList(null);
  }

  String execList;
  String token;
  void NextToken()
  {
    int startPos = 0;
    while (startPos < execList.length() && execList.charAt(startPos) == ' ')
      startPos++;
    int endPos = startPos;
    while (endPos < execList.length() && execList.charAt(endPos) != ' ')
      endPos++;
    token = execList.substring(startPos, endPos);
    execList = execList.substring(endPos);
  }

  void DetermineBounds(double x, double y)
  {
    if (xmin > x) xmin = x;
    if (xmax < x) xmax = x;
    if (ymin > y) ymin = y;
    if (ymax < y) ymax = y;
  }
  int ProjectX(double x)
  {
    return (int)(graphx + graphWidth * (x - xmin) / (xmax - xmin));
  }
  int ProjectY(double y)
  {
    return (int)(graphy + graphHeight * (1.0 - (y - ymin) / (ymax - ymin)));
  }

  void RunCallList(Graphics g)
  {
    try
    {
      Graphics2D g2d = null;
      if (g != null)
      {
        if (g instanceof Graphics2D)
        {
          g2d = (Graphics2D)g;
        }
      }

      Color penColor = new Color(0,0,0);
      Color fillColor = new Color(0,0,0);
      if (g != null)
      {
        g.setColor(penColor);
      }
      execList = iCallList;
      NextToken();
      while (token.length() > 0)
      {
        if (token.equals("lines2d"))
        {
          int i;
          NextToken();
          int nr = Integer.parseInt(token);
          NextToken();
          double x2,y2=0;
          x2 = Float.parseFloat(token);
          NextToken();
          y2 = Float.parseFloat(token);
          if (g == null)
          {
            DetermineBounds(x2, y2);
          }
          double x1,y1;
          for (i=1;i<nr;i++)
          {
            x1 = x2;
            y1 = y2;
            NextToken();
            x2 = Float.parseFloat(token);
            NextToken();
            y2 = Float.parseFloat(token);
            if (g == null)
            {
              DetermineBounds(x2, y2);
            }
            if (g != null)
            {
              g.drawLine(ProjectX(x1), ProjectY(y1), ProjectX(x2), ProjectY(y2));
            }
          }
        }
        else if (token.equals("rectangle2d"))
        {
          NextToken();
          int flags = Integer.parseInt(token);
          NextToken();
          float x0 = Float.parseFloat(token);
          NextToken();
          float y0 = Float.parseFloat(token);
          NextToken();
          float x1 = Float.parseFloat(token);
          NextToken();
          float y1 = Float.parseFloat(token);

          if (g == null)
          {
            DetermineBounds(x0, y0);
            DetermineBounds(x1, y1);
          }
          if (g != null)
          {
            int xPix0 = ProjectX(x0);
            int yPix0 = ProjectY(y0);
            int xPix1 = ProjectX(x1);
            int yPix1 = ProjectY(y1);

            if (xPix1<xPix0)
            {
              int swap = xPix1;
              xPix1 = xPix0;
              xPix0 = swap;
            }
            if (yPix1<yPix0)
            {
              int swap = yPix1;
              yPix1 = yPix0;
              yPix0 = swap;
            }

            if ((flags & 1) != 0)
            {
              g.setColor(fillColor);
              g.fillRect(xPix0,yPix0,xPix1-xPix0,yPix1-yPix0);
              g.setColor(penColor);
            }
            if ((flags & 2) != 0)
            {
              g.drawRect(xPix0,yPix0,xPix1-xPix0,yPix1-yPix0);
            }
          }
        }
        else if (token.equals("pencolor"))
        {
          NextToken();
          int red = Integer.parseInt(token);
          NextToken();
          int green = Integer.parseInt(token);
          NextToken();
          int blue = Integer.parseInt(token);
          if (g != null)
          {
            penColor = new Color(red, green, blue);
            g.setColor(penColor);
          }
        }
        else if (token.equals("fillcolor"))
        {
          NextToken();
          int red = Integer.parseInt(token);
          NextToken();
          int green = Integer.parseInt(token);
          NextToken();
          int blue = Integer.parseInt(token);
          if (g != null)
          {
            fillColor = new Color(red, green, blue);
          }
        }
        else if (token.equals("pensize"))
        {
          NextToken();
          float width = Float.parseFloat(token);
          if (g != null)
          {
            if (g2d != null)
            {
              g2d.setStroke(new BasicStroke(width, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            }
          }
        }
        else
        {
          //TODO raise an exception here
          return;
        }
        NextToken();
      }
    }
    catch (Exception e)
    {
    //TODO handle exception here
    }
  }

  int graphx = 0;
  int graphy = 0;
  int graphWidth = 10;
  int graphHeight = 10;
  int axesFontHeight = 12;
  FontMetrics fontMetrics = null;
  int exampleWidth = 48;
  void DetermineGraphBounds(int xleft, int ytop, Dimension d)
  {
    if (fontMetrics != null)
    {
      exampleWidth = fontMetrics.stringWidth("100000");
    }
    graphx = xleft + exampleWidth;
    graphy = ytop + axesFontHeight;
    graphWidth = d.width - (3 * exampleWidth) / 2;
    graphHeight = d.height - 3 * axesFontHeight;
  }
  public void paint(Graphics g, int xleft, int ytop, Dimension d)
  {
    Shape clip = g.getClip();

    Rectangle r = null;
    if (clip != null)
    {
      r = clip.getBounds();
    }
    Graphics2D g2d = null;
    if (g instanceof Graphics2D)
    {
      g2d = (Graphics2D)g;
    }
    if (g2d != null)
    {
      g2d.addRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON));
    }
    int clipHeight = d.height;
    if (r != null)
    {
      if (ytop+clipHeight > r.y+r.height)
      {
        clipHeight = r.y+r.height-ytop;
      }
    }
    g.setClip(xleft, ytop, d.width, clipHeight); 
    
    // Erase the previous image
    g.setColor(Color.white);
    g.fillRect(xleft, ytop, d.width, d.height);

    Font font;
    font = new Font("Verdana", Font.PLAIN, axesFontHeight);
    g.setFont(font);
    fontMetrics = g.getFontMetrics(font);
    DetermineGraphBounds(xleft, ytop, d);

    Color grey = new Color(164, 164, 164);

    double x, y;

    PlotRange xRange = new PlotRange(xmin, xmax, d.width / ((3 * exampleWidth) / 2));
    int xtick = ((int)(xmin / xRange.TickSize() - 1));
    if (xRange.TickSize() * xtick < xmin)
      xtick = xtick + 1;
    double xstart = xRange.TickSize() * xtick;
    {
      g.setColor(Color.black);
      for (x = xstart; x <= xmax; x += xRange.TickSize())
      {
        int xPix = (int)(graphx + graphWidth * (x - xmin) / (xmax - xmin));
        g.setColor(grey);
        g.drawLine(xPix, graphy, xPix, graphy + graphHeight);
        g.setColor(Color.black);
        String num = xRange.Format(xtick);
        int numWidth = fontMetrics.stringWidth(num);
        g.drawString(num, xPix - numWidth / 2, graphy + graphHeight + fontMetrics.getAscent());
        xtick++;
      }

      PlotRange yRange = new PlotRange(ymin, ymax, d.height / (axesFontHeight * 2));
      int ytick = ((int)(ymin / yRange.TickSize() - 1));
      if (yRange.TickSize() * ytick < ymin)
        ytick = ytick + 1;
      double ystart = yRange.TickSize() * ytick;
      for (y = ystart; y <= ymax; y += yRange.TickSize())
      {
        int yPix = (int)(graphy + graphHeight * (ymax - y) / (ymax - ymin));
        g.setColor(grey);
        g.drawLine(graphx, yPix, graphx + graphWidth, yPix);
        g.setColor(Color.black);
        String num = yRange.Format(ytick);
        int numWidth = fontMetrics.stringWidth(num);
        g.drawString(num, graphx - numWidth - 8, yPix + fontMetrics.getAscent() - (axesFontHeight) / 2);
        ytick++;
      }
    }
    
    int graphClipHeight = graphHeight;
    if (r != null)
    {
      if (graphy+graphClipHeight > r.y+r.height)
      {
        graphClipHeight = r.y+r.height-graphy;
      }
    }
    
    g.setClip(graphx,graphy,graphWidth,graphClipHeight); 
    RunCallList(g);
    g.setClip(xleft, ytop, d.width, clipHeight); 
    g.setColor(Color.black);
    if (g2d != null)
    {
      g2d.setStroke(new BasicStroke(3.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
    }
    g.drawRect(graphx, graphy, graphWidth, graphHeight);
    g.setClip(clip);
  }

  public double xmin, ymin, xmax, ymax;
  String iCallList;


  
  /*
   * Determine the ticks of the graph. The calling routine should first determine the minimum and maximum values, and
   * the number of steps (based on size of the axis to draw relative to font size).
   * 
   * Steps will always be m*10^n, for some suitable n, with m either 1, 2 or 5.
   */
  class PlotRange
  {
    public PlotRange(double aMinValue, double aMaxValue, int aMaxSteps)
    {
      iMinValue = aMinValue;
      iMaxValue = aMaxValue;
      iMaxSteps = aMaxSteps;

      //TODO handle zero length range
      double range = aMaxValue - aMinValue;
      iN = (int)(Math.log(range) / Math.log(10) - 1);
      iN = iN - 1;
      iStep = 1;
      for (; ; )
      {
        double tickSize = TickSize();
        int nrSteps = (int)(range / tickSize);
        if (nrSteps <= aMaxSteps)
          break;
        switch (iStep)
        {
          case 1:
            iStep = 2;
            break;
          case 2:
            iStep = 5;
            break;
          case 5:
            iN++;
            iStep = 1;
            break;
        }
      }
    }
    public double TickSize()
    {
      return iStep * Math.pow(10, iN);
    }

    public String Format(int tick)
    {
      String result = "";
      int fct = tick * iStep;
      if (iN >= 0 && iN < 3)
      {
        if (iN > 0)
          fct = fct * 10;
        if (iN > 1)
          fct = fct * 10;
        result = "" + fct;
      }
      else
      {
        int n = iN;
        if (fct == 10 * (fct / 10))
        {
          fct /= 10;
          n += 1;
        }
        String ex = "";
        if (n != 0 && tick != 0)
          ex = "e" + n;
        result = "" + fct + ex;
      }
      return result;
    }

    double iMinValue;
    double iMaxValue;
    int iMaxSteps;

    public int iN;
    public int iStep;
  }

}
