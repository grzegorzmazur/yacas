package net.sf.yacas;



















class YacasNotebook
{
  Hints the_hints;
  public  void draw(YacasGraphicsContext  g)
  {
    g.SetColor(0,255,0);
    g.FillRect(10,10,30,30);

//TODO remove
    SelectSymbol selecter = new SelectSymbol(11,16);
    selecter.draw(5,22,g);

    the_hints = new Hints();

//#include "../cplusplus/hintsjava"

    HintWindow hints = new HintWindow(12);
    hints.AddLine("First line",g);
    hints.AddLine("Second line",g);
    hints.AddDescription("First description",g);
    hints.AddDescription("Second description",g);
    hints.draw(5,100,g);


  }
};
