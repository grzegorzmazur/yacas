package net.sf.yacas;


interface TResult
{
  public int NrLines(YacasGraphicsContext  aGraphicsContext, int width) ;
  public void draw(YacasGraphicsContext  aGraphicsContext, int current_word, int width, int height, int red, int green, int blue);
};

