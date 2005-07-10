
import java.io.*;
import java.net.*;
import java.awt.*;

class YacasNotebookComponent // extends java.awt.Component
{
  final public static int MAX_ROWS = 30;
  TSentence iRows[] = new TSentence[MAX_ROWS];
  int iNrRows;
  TSentence iSentence = new TSentence();
  int current_word;
  HintWindow hintWindow = null;
  SelectSymbol symbolWindow = null;
  Hints the_hints = new Hints();
  public boolean dirty = true;

  LispOutput stdoutput = null;
  CYacas yacas = null;
  StringBuffer outp = new StringBuffer();

  YacasNotebookComponent(String hintsfilename/*TTODO remove ,String servername,int serverport*/,java.applet.Applet applet)
  {
    iNrRows = 0;
    current_word = 0;
    InsertSpace();

    LoadHints(hintsfilename);
//    enableEvents(0xffffffff);

    stdoutput = new StringOutput(outp);
    yacas = new CYacas(stdoutput);
    yacas.env.iCurrentInput = new CachedStdFileInput(yacas.env.iInputStatus);


    {
      String docbase = applet.getDocumentBase().toString();
      if (docbase.substring(0,4).equals("file"))
      {
        int pos = docbase.lastIndexOf("/");
        String zipFileName = docbase.substring(0,pos+1)+"scripts.zip";
        try
        {
        //  java.util.zip.ZipFile z = new java.util.zip.ZipFile(new File(new java.net.URI("file:/Users/ayalpinkus/projects/JavaYacas/tempscripts.zip")));
          java.util.zip.ZipFile z = new java.util.zip.ZipFile(new File(new java.net.URI(zipFileName)));
          LispStandard.zipFile = z;
          //out.println("Succeeded in finding "+zipFileName);
        }
        catch(Exception e)
        {
          //out.println("Failed to find scripts.zip");
          //out.println(""+zipFileName+" : \n");
          //out.println(e.toString());
        //  return;
        }
      }
    }

    int i;
    i=1;
    while (true)
    {
      String argn = "init"+i;
      String s = applet.getParameter(argn);
      if (s == null) break;
      s = unescape(s);
      PerformRequest("Init>",s);
      i++;
    }

  }

  private String unescape(String s)
  {
    StringBuffer buf = new StringBuffer();
    int i,nr=s.length();
    for(i=0;i<nr;i++)
    {
      if (s.charAt(i) == '\'' && s.charAt(i+1) == '\'')
      {
        buf.append('\"');
        i++;
      }
      else
      {
        buf.append(s.charAt(i));
      }
    }
    return buf.toString();
  }

  void DeleteLastDigit()
  {
    if (iSentence.DeleteLastDigit(current_word) != 0)
    {
      dirty=true;
    }
  }
  
  void InsertSpace()
  {
    iSentence.search_start = 0;
    iSentence.InsertSpace(current_word);
  }

  Dimension canvasSize = null;
  Point topLeft = null;
  void setBounds(int x, int y, int width, int height)
  {
    topLeft = new Point(x,y);
    canvasSize = new Dimension(width,height);
  }
  Dimension getSize()
  {
    return canvasSize;
  }

  public void paint(Graphics g)
  {
    dirty=false;
/*
    Dimension d = getSize();
System.out.println("size = "+d.getWidth()+" by "+d.getHeight());
    g.setColor(new Color(224,224,224));
		g.fillRect(0,0,(int)d.getWidth(),(int)d.getHeight());
    g.setColor(Color.black);
		g.drawRect(0,0,(int)d.getWidth()-1,(int)d.getHeight()-1);

*/

System.out.println("Draw called");
    YacasGraphicsContext context = new YacasGraphicsContext(g,topLeft.x,topLeft.y);
    Dimension d = getSize();
System.out.println("size = "+d.getWidth()+" by "+d.getHeight());
    g.setColor(new Color(224,224,224));
		g.fillRect(topLeft.x,topLeft.y,(int)d.getWidth(),(int)d.getHeight());
    g.setColor(Color.black);
		g.drawRect(topLeft.x,topLeft.y,(int)d.getWidth()-1,(int)d.getHeight()-1);
    int progress = 0;
    int i;
    context.SetFontSize(1,12);  
    {
      int ypos = (int)d.getHeight()-((int)iSentence.NrLines(context,(int)d.getWidth()))*(int)context.FontHeight();
      for (i=iNrRows-1;i>=0;i--)
      {
//        printf("%d: %d\n",i,ypos);
        if (ypos+(iRows[i].NrLines(context,(int)d.getWidth()))*context.FontHeight()<0)
          break;
        iRows[i].draw(context,-1,(int)d.getWidth(),ypos,255,0,0);
        ypos -= (iRows[i].NrLines(context,(int)d.getWidth()))*context.FontHeight();
      }
    }
  
    int nr_lines = iSentence.NrLines(context,(int)d.getWidth())-1;
    int nr_total_lines = nr_lines;
    
    iSentence.draw(context,current_word,(int)d.getWidth(),(int)d.getHeight(),0,0,0);
    if (symbolWindow != null)
    {
      symbolWindow.draw(5,(int)(d.getHeight()-context.FontHeight()-symbolWindow.height(context)-nr_total_lines*context.FontHeight()),context);
    }
    else if (hintWindow != null)
    {
      hintWindow.draw(5,(int)(d.getHeight()-context.FontHeight()-nr_total_lines*context.FontHeight()),context);
    }

  }




  HintWindow CreateHints(int fontsize)
  {
    return new HintWindow(fontsize);
  }
  
  void AddHintLine(HintWindow hints, String aText, String aDescription)
  {
      hints.AddLine(aText);
      if (aDescription.length() > 0)
        hints.AddDescription(aDescription);
  }
  
  
  HintWindow TryToHint(String text, int length)
  {
    HintWindow hints = null;
    int nrhints = the_hints.nrHintTexts;
    int i,start;
    start = 0;//hoffsets[(unsigned char)text[0]];
    if (start<0)
        return null;
    for (i = start;i<nrhints;i++)
    {
        if (text.charAt(0) > the_hints.hintTexts[i].base.charAt(0))
      {
        continue;
      }
      if (text.charAt(0) < the_hints.hintTexts[i].base.charAt(0))
      {
        continue;
      }
      int baselen = the_hints.hintTexts[i].base.length();
      if (length == baselen)
      {
        if (text.substring(0,baselen).equals(the_hints.hintTexts[i].base))
        {
          if (hints == null)
              hints = CreateHints(12 /*iDefaultFontSize*/);
          AddHintLine(hints, the_hints.hintTexts[i].hint,the_hints.hintTexts[i].description);
        }
      }
    }
    return hints;
  }



  void FindWord()
  {
    hintWindow = null;
    if (iSentence.FindWord(the_hints,current_word) != 0)
    {
      hintWindow = TryToHint(iSentence.words[current_word].word, iSentence.words[current_word].word.length());
    }
  }
  
  void AppendDigit(char digit)
  {
System.out.println("enter AppendDigit");
    if (iSentence.AppendDigit(digit,current_word) != 0)
    {
      FindWord();
    }
    dirty=true;  
  }


  void  LoadHints(String filename)
  {
System.out.println("hints file : ["+filename+"]");
    CDataReader file = new CDataReader();
    int opened = 0;
    try 
    {
  
  //System.out.println("code base "+iApplet.getCodeBase().getFile());
  //System.out.println("doc base "+iApplet.getDocumentBase().toString());
  //iApplet.getDocumentBase().toString()+".lev"
      URL url = new URL(filename);
      opened = file.Open(url);
    }
    catch (Exception e)
    {
    }
    if (opened != 0)
    {
System.out.println("hints opened successfully");
      String line = file.ReadLine();
      String[] tokens = new String[16];
      int nrTokens = 0;
      while (line != null)
      {
//        System.out.println("LINE : "+line);
        if (line.substring(0,2).equals("~~"))
          break;
        int i=0;
        nrTokens = 0;
        while (i<line.length())
        {
          int start = i;
          while (line.charAt(i) != '~') i++;
          tokens[nrTokens] = line.substring(start,i);
          nrTokens++;
          i++;
        }
//        System.out.println("DIGITS : "+tokens[1]);
        if (nrTokens>4)
        {
          HintItem hi = new HintItem();
          hi.digits = tokens[1];
          hi.base = tokens[2];
          hi.hint = tokens[3];
          hi.description = tokens[4];
          the_hints.hintTexts[the_hints.nrHintTexts] = hi;
          the_hints.nrHintTexts++;
        }

        line = file.ReadLine();
      }
      file.Close();
    }
    else
    {
System.out.println("could not read hints");
    }

//TODO
  }

	/* This method is called whenever a numeric button (0-9) is pushed. */
	public void button_0(char c) 
	{

System.out.println("enter button_0");
    int next_word = 0;
    if (c == '1' || 
       (iSentence.words[current_word].digits.length()>0 && iSentence.words[current_word].digits.charAt(0) == '1'))
    {
      if (iSentence.words[current_word].digits.length() > 0 && iSentence.words[current_word].digits.charAt(0) != c)
      {
        next_word = 1;
      }
    }
    if (next_word != 0)
    {
      current_word++;
      InsertSpace();
    }
    AppendDigit(c); 
	}
	public void button_clear() 
	{
    DeleteLastDigit();
	}
	public void button_left() 
	{
    if (symbolWindow != null)
    {
      symbolWindow.ix--;
      if (symbolWindow.ix<0)
        symbolWindow.ix+=symbolWindow.iwidth;
      dirty=true;
    }
    else if (current_word > 0)
    {
      current_word--;
      hintWindow = TryToHint(iSentence.words[current_word].word, iSentence.words[current_word].word.length());
      dirty=true;
    }
	}
	public void button_right() 
	{
    if (symbolWindow != null)
    {
      if (symbolWindow.iy<symbolWindow.iwidth-1)
      {
        symbolWindow.ix++;
        if (symbolWindow.ix>=symbolWindow.iwidth)
          symbolWindow.ix-=symbolWindow.iwidth;
        dirty=true;
      }
    }
    else
    {
      //TODO bounds check!
      current_word++;
      hintWindow = null;
      if (current_word == iSentence.nr_words)
      {
        InsertSpace();
      }
      else
      {
        hintWindow = TryToHint(iSentence.words[current_word].word, iSentence.words[current_word].word.length());
      }
      dirty=true;
    }
	}
	public void button_up() 
	{
    if (symbolWindow != null)
    {
      symbolWindow.iy--;
      if (symbolWindow.iy<0)
        symbolWindow.iy+=symbolWindow.iheight;
      dirty=true;
    }
    else
    if (hintWindow != null)
    {
      if (hintWindow.iCurrentPos >0)
      {
        hintWindow.iCurrentPos--;
        dirty=true;
      }
    }
	}
	public void button_down() 
	{
    if (symbolWindow != null)
    {
      symbolWindow.iy++;
      if (symbolWindow.iy>=symbolWindow.iheight)
        symbolWindow.iy-=symbolWindow.iheight;
      dirty=true;
    }
    else if (hintWindow != null)
    {
      if (hintWindow.iCurrentPos < hintWindow.iNrLines-1)
      {
        hintWindow.iCurrentPos++;
        dirty=true;
      }
    }
	}
  
  int chtype(char c)
  {
    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'))
      return 0;
    return 1;
  }

  int AppendSentence(String src,TSentence sentence,int current_word)
  {
    String trg = new String();
    int srci = 0;
    int cur_type = chtype(src.charAt(srci));
    while (srci < src.length())
    {
      if (chtype(src.charAt(srci)) == cur_type)
      {
        trg = trg + src.charAt(srci);
      }
      else
      {
        sentence.words[current_word].word = trg;
        cur_type = chtype(src.charAt(srci));
        current_word++;
        sentence.InsertSpace(current_word);
        trg = new String();//sentence.words[current_word].word;
        trg = trg + src.charAt(srci);
      }
      srci++;
    }
    sentence.words[current_word].word = trg;
    return current_word;
  }

  
	public void button_center() 
	{
    if (symbolWindow != null)
    {
      if (symbolWindow.symbols[symbolWindow.iy*symbolWindow.iwidth+symbolWindow.ix].equals("1"))
      {
        AppendDigit('1');
      }
      else
      {
        current_word++;
        InsertSpace();
        iSentence.words[current_word].word = new String(symbolWindow.symbols[symbolWindow.iy*symbolWindow.iwidth+symbolWindow.ix]);
        current_word++;
        InsertSpace();
      }
      symbolWindow = null;
      dirty=true;
    }
    else if (hintWindow != null)
    {
      if (hintWindow.iCurrentPos < hintWindow.iNrLines)
      {
        int curw = current_word;
        String src = hintWindow.iText[hintWindow.iCurrentPos];
        current_word = AppendSentence(src,iSentence,current_word);
        hintWindow = null;
        if (curw != current_word)
        {
          current_word = curw;
          if (iSentence.words[current_word+1].word.charAt(0) == '(')
            current_word+=2;
        }
        dirty=true;
      }
    }
	}
	public void button_space() 
	{
    if (iSentence.words[current_word].word.charAt(0) >= '0' && iSentence.words[current_word].word.charAt(0) <= '9')
    {
      AppendDigit('0');
    }
    else
    {
      current_word++;
      InsertSpace();
      current_word++;
      InsertSpace();
    }
    dirty=true;
	}
	public void button_symbol() 
	{
    if (symbolWindow != null)
    {
      symbolWindow = null;
    }
    else
    {
      symbolWindow = new SelectSymbol(11,16);
    }
    dirty=true;
	}
	public void button_enter() 
	{
    if (iNrRows == MAX_ROWS)
    {
      int i;
      for (i=0;i<iNrRows-1;i++)
        iRows[i] = iRows[i+1];
      iNrRows--;
    }
    String input = new String();
    {
      int i;
      for (i=0;i<iSentence.nr_words;i++)
      {
        input = input + iSentence.words[i].word;
      }
    }
    iRows[iNrRows] = iSentence;
    iSentence = new TSentence();
    TSentence result = new TSentence();
    iRows[iNrRows].iResult = result;
    result.InsertSpace(0);
//    result.words[0] = new TWord();
//    result.nr_words = 1;
    String resultStr = PerformRequest("Out> ",input);
    AppendSentence(resultStr,result,0);
//      result.words[0].word = new String("Result");

  
    iNrRows++;
    iSentence.nr_words = 0;
    current_word = 0;
    iSentence.InsertSpace(0);
    hintWindow = null;
    symbolWindow = null;
    dirty=true;
	}
	public void button_nextword() 
	{
    iSentence.search_start++;
    while (iSentence.search_start < the_hints.nrHintTexts && iSentence.words[current_word].word.equals(the_hints.hintTexts[iSentence.search_start].base))
      iSentence.search_start++;
    FindWord();
    dirty=true;
	}








  String PerformRequest(String outputPrompt,String inputLine)
  {
    boolean succeed = false;
    String result = "";
/*TODO fixme
    if (inputLine.startsWith("restart"))
    {
      stop();
      out.println("Restarting");
      start();
      return;
    }
*/
    {
      outp.delete(0,outp.length());
      String response = yacas.Evaluate(inputLine);
      if (outp.length() > 0)
      {
        result = result+outp.toString();
      }
      if (yacas.iError != null)
      {
        result = result+("ERROR> "+yacas.iError);
      }
      result = result+outputPrompt+response;
      succeed = true;
    }
    {
      if (!succeed) 
      {
        result = result+"Request failed";
      }
    }
    return result;
  }



};
