
package net.sf.yacas;

import java.applet.*;


/* This little applet should facilitate communication between Java and Javascript.
 * The idea is to allow Javascript to set data in this applet at various times, and
 * for the main Yacas console to then get that data at startup, when it is loaded.
 * 
 * The Yacas console thus does not need to be loaded always, but this applet should
 * always be there.
 */
 
 
public class DatahubApplet extends Applet
{
  public String getProgramToLoad()
  {
    return programToLoad;
  }
  public void setProgramToLoad(String p)
  {
    programToLoad = p;
  }
  // Empty program by default
  static String programToLoad = "True;";
}
 
