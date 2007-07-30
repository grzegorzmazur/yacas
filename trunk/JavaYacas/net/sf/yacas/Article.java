package net.sf.yacas;

class Article
{
  public Article(String aArticle)
  {
    SetArticle(aArticle);
  }
  public void SetArticle(String aArticle)
  {
    iArticle = aArticle;
    testCode = "";
    codeBody = "";
    articleBody = "";
    nrExamples = 0;
    processBody(aArticle);
  }
  public String iArticle;
  public String testCode = "";
  public String codeBody = "";
  public String articleBody = "";
  public String[] examples = new String[100];
  public int nrExamples = 0;
  int currentExample = 0;
  public String getExample()
  {
    String result = "\"No example defined\"";
    if (nrExamples>0)
    {
      result = examples[currentExample];
      currentExample++;
      if (currentExample == nrExamples)
      {
        currentExample = 0;
      }
    }
    return result;
  }
  void processText(String aString)
  {
    int pos = aString.indexOf("\n\n");
    if (pos == -1)
    {
      articleBody = articleBody + aString;
      return;
    }
    else
    {
      articleBody = articleBody + aString.substring(0, pos) + "\n<br /><br />\n";
      processText(aString.substring(pos + 2));
    }
  }
  void processBody(String aString)
  {
    while (aString.length() > 0)
    {
      int pos = aString.indexOf("{{");
      if (pos == -1)
      {
        processText(aString);
        return;
      }
      processText(aString.substring(0, pos));
      aString = aString.substring(pos + 2);
      pos = aString.indexOf(":");
      String name = aString.substring(0, pos);
      aString = aString.substring(pos + 1);
      String close = ":" + name + "}}";
      pos = aString.indexOf(close);
      String data = aString.substring(0, pos);
      aString = aString.substring(pos + close.length());
      if (name.equals("title"))
      {
        articleBody = articleBody + "<h1>" + data + "</h1>";
      }
      else if (name.equals("code"))
      {
        codeBody = codeBody + data;
        articleBody = articleBody + "<table width=\"100%\"><tr><td width=100% bgcolor=\"#DDDDEE\"><pre>" + data + "</pre></tr></table>";
      }
      else if (name.equals("test"))
      {
        testCode = testCode + data;
      }
      else if (name.equals("expr"))
      {
        articleBody = articleBody + "<tt><b>" + data + "</b></tt>";
      }
      else if (name.equals("example"))
      {
        if (nrExamples < 100)
        {
          examples[nrExamples] = data;
          nrExamples++;
        }
      }
    }
  }
}

