

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.io.*;
import java.net.*;

public class ConsoleApplet extends Applet implements KeyListener
{
  static AppletOutput out;
  static Socket client;
  static String serverAddress;
  static int serverPort;
  /// Applet initialization
  public void init() 
  {
    setLayout (null);
    addKeyListener(this);

    out = new AppletOutput();
    
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
    ResetInput();
    try
    {
      client = new Socket(serverAddress,serverPort);
    }
    catch (Exception e)
    {
      out.println(e);
    }
    out.println("");
    out.println("Welcome to the Yacas console applet!");
    out.println("");
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

        PerformRequest(outputPrompt,inputLine);
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
  static void PerformRequest(String outputPrompt,String inputLine)
  {
    boolean succeed = false;
    try
    {
      BufferedOutputStream buffered = new BufferedOutputStream(client.getOutputStream());
      DataOutputStream outbound = new DataOutputStream(buffered);
      DataInputStream inbound = new DataInputStream(client.getInputStream());
      outbound.writeBytes(inputLine);
      outbound.flush();
      String responseLine;
      while ((responseLine = inbound.readLine()) != null)
      {
        AddLineStatic(outputPrompt+responseLine);
        break;
      }
//      outbound.close();
//      inbound.close();
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
      if (!succeed) out.println("Request failed");
    }
  }

  final static int nrLines =  60;
  static String lines[] = new String[nrLines];
  static int currentLine=0;
  void AddLine(String text)
  {
    AddLineStatic(text);
    repaint(0);
  }

  static void AddLineStatic(String text)
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
  static final int inset = 5;
  
  static String inputPrompt = "In> ";
  static String outputPrompt = "Out> ";

  static final int fontHeight = 12;
	private Font font = new Font("Monaco", /*Font.ITALIC + Font.BOLD*/Font.PLAIN, fontHeight);
//	private Font font = new Font("times", /*Font.ITALIC + Font.BOLD*/Font.PLAIN, fontHeight);
//	private Font font = new Font("serif", /*Font.ITALIC + Font.BOLD*/Font.PLAIN, fontHeight);

  private static final int nrHistoryLines = 50;
  private String history[] = new String[nrHistoryLines]; 
  int currentHistoryLine = 0;
  int historyBrowse = 0;

  static boolean inputDirty = true;
  static boolean outputDirty = true;


  class AppletOutput 
  {
    public void write(int c) throws IOException
    {
      if (c == '\n')
      {
        ConsoleApplet.AddLineStatic(buffer.toString());
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

