package net.sf.yacas;




class TSentence implements TResult
{
  public  TSentence() 
  {
    nr_words = 0;
    iResult=null;
    search_start = 0;
  }
  public  int DeleteLastDigit(int current_word)
  {
    if (nr_words>0)
    {
      int nr = words[current_word].digits.length();
      if (nr>0)
      {
        words[current_word].digits = words[current_word].digits.substring(0,nr-1);
        if (words[current_word].word.length()>nr-1);
          words[current_word].word   = words[current_word].word.substring(0,nr-1);
        search_start = 0;
        return 1;
      }
      else
      {
        if (nr_words>current_word && nr_words>1)
        {
          {
            int j;
            for (j=current_word;j<nr_words-1;j++)
            {
              words[j] = words[j+1];
            }
            words[j] = null;
          }
        
//          memmove(&words[current_word],&words[current_word+1],(nr_words-current_word-1)*sizeof(TWord));
          nr_words--;
          if (current_word > 0)
            current_word--;
          search_start = 0;
          return 1;
        }
        else
        {
          (words[current_word].word) = (" ");
          search_start = 0;
          return 1;
        }
      }
    }
    return 0;
  }
  public  void InsertSpace(int current_word)
  {
    {
      int j;
      for (j=nr_words-1;j>=current_word;j--)
      {
        words[j+1] = words[j];
      }
    }
    words[current_word] = new TWord();

//    memmove(&words[current_word+1],&words[current_word],(nr_words-current_word)*sizeof(TWord));
    words[current_word].word = new String(" ");
    words[current_word].digits = new String("");
    nr_words++;
  }
  public  int FindWord(Hints aHints, int current_word)
  {
    (words[current_word].word) = ( " ");
    int nr = (words[current_word].digits).length();
    if (nr>0)
    {
      int i;
      while (true)
      {
        for (i=search_start;i<aHints.nrHintTexts;i++)
        {
          if (nr <= (aHints.hintTexts[i].digits).length()-1 && 
              words[current_word].digits.substring(0,nr).equals(aHints.hintTexts[i].digits.substring(0,nr)))
          {
            search_start = i;
            words[current_word].word = aHints.hintTexts[i].base;
            return 1;
          }
        }
        if (i == aHints.nrHintTexts && search_start != 0)
        {
          search_start = 0;
//          goto REDO;
        }
        else
        {
          break;
        }
      }
    }
    return 0;
  }
  public  int NrLines(YacasGraphicsContext  aGraphicsContext, int width)
  {
    int progress = 0;
    int nr_lines = 1;
    int i;
    for (i=0;i<nr_words;i++)
    {
      if (5+progress+(int)aGraphicsContext.TextWidthInPixels(words[i].word)>width)
      {
        nr_lines++;
        progress = 0;
      }
      progress += (int)aGraphicsContext.TextWidthInPixels(words[i].word);
    }
    if (iResult != null)
    {
      nr_lines += iResult . NrLines(aGraphicsContext,width);
    }
    return nr_lines;
  }
  public  int AppendDigit(char digit, int current_word)
  {
    int nr = words[current_word].digits.length();
    words[current_word].digits = words[current_word].digits + digit;
    if (words[current_word].word.equals(" "))
    {
      words[current_word].word = new String();
      words[current_word].word = words[current_word].word + digit;
      return 0;
    }
    else if (words[current_word].word.length()>0 && words[current_word].word.charAt(0) >= '0' && words[current_word].word.charAt(0) <= '9')
    {
      words[current_word].word = words[current_word].word + digit;
      return 0;
    }
    else
    {
      return 1;
    }















  }
  public  void draw(YacasGraphicsContext  aGraphicsContext, int current_word, int width, int height, int red, int green, int blue)
  {
    int nr_lines = NrLines(aGraphicsContext,width)-1;
    int progress = 0, i;
    for (i=0;i<nr_words;i++)
    {
      if (5+progress+(int)aGraphicsContext.TextWidthInPixels(words[i].word)>width)
      {
        nr_lines--;
        progress = 0;
      }
      if (i != current_word)
      {
        aGraphicsContext.SetColor(red,green,blue);
        aGraphicsContext.DrawText(5+progress,height-5-nr_lines*aGraphicsContext.FontHeight(),words[i].word);
      }
      else
      {
        aGraphicsContext.SetColor(0,0,0);
        if ((words[i].digits).length() < (words[i].word).length())
        {
          String ln = words[i].word.substring(0,words[i].digits.length());




          int offset = (int)aGraphicsContext.TextWidthInPixels(ln);
          aGraphicsContext.DrawText(5+progress,height-5-nr_lines*aGraphicsContext.FontHeight(),ln);
          aGraphicsContext.SetColor(128,128,128);
          ln = words[i].word.substring(words[i].digits.length(),words[i].word.length());
//          (ln) = (&words[i].word[(words[i].digits).length()]);
          aGraphicsContext.DrawText(5+progress+offset,height-5-nr_lines*aGraphicsContext.FontHeight(),ln);
        }
        else
        {
          aGraphicsContext.DrawText(5+progress,height-5-nr_lines*aGraphicsContext.FontHeight(),words[i].word);
        }
  
        aGraphicsContext.SetColor(0, 0, 0);
        aGraphicsContext.DrawLine(
               5+progress, height-5-nr_lines*aGraphicsContext.FontHeight(),
               5+progress+aGraphicsContext.TextWidthInPixels(words[i].word), height-5-nr_lines*aGraphicsContext.FontHeight());
      }
      progress += (int)aGraphicsContext.TextWidthInPixels(words[i].word);
    }
    if (iResult != null)
    {
      iResult . draw(aGraphicsContext, current_word, width, height, 0,0,255);
    }
  }
  public  void DebugPrint(YacasGraphicsContext  aGraphicsContext, int width)
  {
  }
  
  public  TWord [] words = new TWord[32];
  public  TResult iResult;
  public  int nr_words;
  public  int search_start;
};
  




