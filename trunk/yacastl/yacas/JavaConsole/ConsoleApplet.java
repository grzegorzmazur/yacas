
/*
Documentation for the applet, starting it here, but has to move to main docs:
just random thoughts for now.

1) typing 'restart' on the command line restarts the system
2) You can perform initialization calls by adding parameters "initN" in the html
   code, where N is a number from 1 upwards, which have to be in consecutive order.
3) add to the history with "historyN" parameters to the applet
*/


import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.io.*;
import java.net.*;

public class ConsoleApplet extends Applet implements KeyListener
{
  AppletOutput out;
  Socket client;
  String serverAddress;
  int serverPort;
  /// Applet initialization
  public void init() 
  {
    setLayout (null);
    addKeyListener(this);

    out = new AppletOutput(this);
    
    serverAddress = getParameter("ADDRESS");
    if (serverAddress == null) serverAddress = new String("127.0.0.1");
    {
      serverPort = 9734;
      String port = getParameter("PORT");
      if (port != null)
        serverPort = Integer.parseInt(port);
    }
    

    ResetInput();

  }

  public void start()
  {  
    out.println("");
    out.println("Welcome to the Yacas console applet!");
    out.println("You can type 'restart' to restart the engine, ");
    out.println("or type 'Example()' to see some examples");
    out.println("Connecting to the server at address "+serverAddress+" on port "+serverPort);
    try
    {
      PerformRequest("Connected: version of engine is ","Atom(Version())");
      out.println("");
    }
    catch (Exception e)
    {
      out.println(e);
    } 

    int i;
    i=1;
    while (true)
    {
      String argn = "init"+i;
      String s = getParameter(argn);
      if (s == null) break;
      s = unescape(s);
      PerformRequest("Init>",s);
      i++;
    }

    i=1;
    while (true)
    {
      String argn = "history"+i;
      String s = getParameter(argn);
      if (s == null) break;
      s = unescape(s);
      history[currentHistoryLine] = s;
      currentHistoryLine++;
      i++;
    }

    ResetInput();

  }
  public void stop()
  {
      try
      {
        client.close();
      }
      catch (Exception e)
      {
      }
      client = null;
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

  private void ResetInput()
  {
    inputLine = "";
    cursorPos = 0;
    historyBrowse = currentHistoryLine;
    inputDirty = true;
  }

  /// Applet destruction
	public void destroy()
  {
  }

  public void keyPressed(KeyEvent e)
  {
    processKeyEvent(e);
  }
  public void keyTyped(KeyEvent e)
  {
//    processKeyEvent(e);
  }
  public void keyReleased(KeyEvent e)
  {
//    processKeyEvent(e);
  }

  protected void processKeyEvent(KeyEvent e)
  {
    if (KeyEvent.KEY_PRESSED == e.getID())    
    {
      if (e.VK_SHIFT == e.getKeyCode()) {return;}
      if (e.VK_CONTROL == e.getKeyCode()) {return;}
      if (e.VK_ALT == e.getKeyCode()) {return;}
      else if (e.VK_HOME == e.getKeyCode())
      {
        cursorPos = 0;  
      }
      else if (e.VK_END == e.getKeyCode())
      {
        cursorPos = inputLine.length();  
      }
      else if (e.VK_LEFT == e.getKeyCode())
      {
        if (cursorPos>0) cursorPos--;  
      }
      else if (e.VK_BACK_SPACE == e.getKeyCode())
      {
        if (cursorPos>0) 
        {
          cursorPos--;  
          inputLine = new StringBuffer(inputLine).delete(cursorPos,cursorPos+1).toString();
        }
      }
      else if (e.VK_ESCAPE == e.getKeyCode())
      {
        ResetInput();
      }
      else if (e.VK_UP == e.getKeyCode())
      {
        String prefix = inputLine.substring(0,cursorPos);
        int i = historyBrowse - 1;
        while (i > 0)
        {
          if (history[i].startsWith(prefix))
            break;
          i--;
        }
        if (i >= 0 && i != historyBrowse && history[i].startsWith(prefix))
        {
          historyBrowse = i;
          inputLine = history[historyBrowse];
        }
      }
      else if (e.VK_DOWN == e.getKeyCode())
      {

        String prefix = inputLine.substring(0,cursorPos);
        int i = historyBrowse + 1;
        while (i < currentHistoryLine)
        {
          if (history[i].startsWith(prefix))
            break;
          i++;
        }
        if (i < currentHistoryLine && history[i].startsWith(prefix))
        {
          historyBrowse = i;
          inputLine = history[historyBrowse];
        }
        else 
        {
          int pos = cursorPos;
          ResetInput();
          inputLine = prefix;
          cursorPos = pos;
        }

      }
      else if (e.VK_RIGHT == e.getKeyCode())
      {
        if (cursorPos<inputLine.length()) cursorPos++;  
      }
      else if (e.VK_ENTER == e.getKeyCode())
      {
        history[currentHistoryLine] = inputLine;
        currentHistoryLine++;
        AddLine(inputPrompt+inputLine);

        PerformRequest("Out> ",inputLine);
//        outputDirty = true;
//        inputDirty = true;
        repaint(0);
        ResetInput();
      }
      else
      {
        inputLine = new StringBuffer(inputLine).insert(cursorPos,e.getKeyChar()).toString();
        cursorPos++;
      }
      inputDirty=true;
      repaint(0,getHeight()-2*fontHeight,getWidth(),2*fontHeight);
    }
  }
  void PerformRequest(String outputPrompt,String inputLine)
  {
    boolean succeed = false;
    if (inputLine.startsWith("restart"))
    {
/*
      if (client != null) 
      {
        try { client.close(); } catch(Exception e){out.println(e);}
      }
      client = null;
*/
      stop();
      out.println("Restarting");
      start();
      return;
    }

    if (client == null)
    {
      try
      {
        client = new Socket(serverAddress,serverPort);
      }
      catch (Exception e)
      {
        out.println(e);
      }
    }
    if (client == null)
    {
      out.println("Not connected to the server");
      return;
    }


    try
    {
      BufferedOutputStream buffered = new BufferedOutputStream(client.getOutputStream());
      DataOutputStream outbound = new DataOutputStream(buffered);
      DataInputStream inbound = new DataInputStream(client.getInputStream());
      outbound.writeBytes(inputLine+";");
      outbound.flush();
      String responseLine;
      while ((responseLine = inbound.readLine()) != null)
      {
        if (responseLine.length()>0)
          if (responseLine.charAt(0) == ']') break;
        AddLineStatic(responseLine);
      }

      while ((responseLine = inbound.readLine()) != null)
      {
        if (responseLine.length()>0)
          if (responseLine.charAt(0) == ']') break;
        AddLineStatic(outputPrompt+responseLine);
      }

      succeed = true;
    }
    catch (IOException ex)
    {
      out.println(ex);
    }
    catch (Exception ex)
    {
      out.println(ex);
    }
    finally
    {
      if (!succeed) 
      {
        try
        {
          client.close();
        }
        catch (Exception e)
        {
          out.println(e);
        }
        client=null;
        out.println("Request failed");
      }
    }
  }

  final static int nrLines =  60;
  String lines[] = new String[nrLines];
  int currentLine=0;
  void AddLine(String text)
  {
    AddLineStatic(text);
    repaint(0);
  }

  void AddLineStatic(String text)
  {
    lines[currentLine] = text;
    currentLine = (currentLine+1)%nrLines;
    outputDirty = true;
  }

  /// Drawing current view
	public void paint (Graphics g) 
  {
    FontMetrics metrics = getFontMetrics(font);

// to always redraw everything, make the whole canvas dirty
    inputDirty = outputDirty = true;

		g.setColor(Color.white);
    int yfrom = 0;
    int yto = getHeight()-1;
    if (!outputDirty)
      yfrom += getHeight()-2*fontHeight;
    if (!inputDirty)
      yto -= 2*fontHeight;
      
    g.clearRect(0,yfrom,getWidth(),yto);
		g.setColor(Color.black);
    g.drawRect(0,0,getWidth()-1,getHeight()-1);
		g.setColor(Color.blue);
		g.setFont(font);
    //fontHeight*nrLines + offset = getHeight();

    int y=getHeight()-fontHeight*(nrLines+1);
    int i;
    if (outputDirty)
    {
      for (i=0;i<nrLines;i++)
      {
        if (y+fontHeight>0)
        {
          int index = (currentLine+i)%nrLines;
          if (lines[index] != null) 
            g.drawString(lines[index], inset, y);
        }
        y+=fontHeight;
      }
    }
    else
    {
      y+=nrLines*fontHeight;
    }
    outputDirty = false;
    if (inputDirty)
    {
      if (y+fontHeight>0)
      {
        int promptLength = metrics.stringWidth(inputPrompt);
        g.setColor(Color.red);
        g.drawString(inputPrompt, inset, y);
        g.drawString(inputLine, inset+promptLength, y);
        int cursorLocation = promptLength;
        for (i=0;i<cursorPos;i++)
        {
          cursorLocation += metrics.charWidth(inputLine.charAt(i));
        }
        g.drawLine(inset+cursorLocation,y,inset+cursorLocation,y-fontHeight);
      }
    }
    inputDirty=false;
  }
  String inputLine  = new String();
  int cursorPos = 0;
  final int inset = 5;
  
  final static String inputPrompt = "In> ";
  final static String outputPrompt = "Out> ";

  static final int fontHeight = 12;
	private Font font = new Font("Monaco", /*Font.ITALIC + Font.BOLD*/Font.PLAIN, fontHeight);
//	private Font font = new Font("times", /*Font.ITALIC + Font.BOLD*/Font.PLAIN, fontHeight);
//	private Font font = new Font("serif", /*Font.ITALIC + Font.BOLD*/Font.PLAIN, fontHeight);

  private static final int nrHistoryLines = 50;
  private String history[] = new String[nrHistoryLines]; 
  int currentHistoryLine = 0;
  int historyBrowse = 0;

  boolean inputDirty = true;
  boolean outputDirty = true;


  class AppletOutput 
  {
    public AppletOutput(ConsoleApplet aApplet)
    {
      iApplet = aApplet;
    }
    ConsoleApplet iApplet;
    public void write(int c) throws IOException
    {
      if (c == '\n')
      {
        iApplet.AddLineStatic(buffer.toString());
        buffer = new StringBuffer();
      }
      else
      {
        buffer.append((char)c);
      }
    }
    public void print(String s) 
    {
      try
      {
        int i,nr;
        nr = s.length();
        for (i=0;i<nr;i++)
        {
          write(s.charAt(i));
        }
      }
      catch (IOException e)
      {
      }
    }
    public void println(Exception e) 
    {
      println(e.getMessage());
    }
    public void println(String s) 
    {
      print(s);
      print("\n");
    }
    StringBuffer buffer = new StringBuffer();
  }

}

