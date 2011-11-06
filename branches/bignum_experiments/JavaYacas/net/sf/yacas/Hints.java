package net.sf.yacas;


class Hints
{
  public  Hints()
  {
    nrHintTexts = 0;
  }
  public  int nrHintTexts;

  public  HintItem[] hintTexts = new HintItem[1024];
  int[] hoffsets = new int[256];
};
