package net.sf.yacas;


/**
 * Use this class in order to access the Yacas interpreter from an external application.
 * Usage:
 * import net.sf.yacas.YacasInterpreter;
 * YacasInterpreter interpreter = new YacasInterpreter();
 * String output1 = interpreter.Evaluate("a := 5");
 * String output2 = interpreter.Evaluate("Solve(x*x == a, x)");
 *
 *
 * @author av
 */
public class YacasInterpreter {
 
    private CYacas yacas;
 
    /** Creates a new instance of YacasInterpreter */
    public YacasInterpreter() {
        yacas = new CYacas(new StringOutput(new StringBuffer()));
        boolean scripts_found = loadScripts();
 
        if (!scripts_found) System.err.println("Yacas error: Unable to load yacas.jar");
        yacas.Evaluate("Load(\"yacasinit.ys\");");
 
    }
 
    /** Searches for the file yacas.jar and passes its absolute path to the Yacas interpreter.
     * This method searches in the classpath (declared i.e. in MANIFEST.MF) for the file yacasinit.ys.
     * yacasinit.ys is inside yacas.jar.
     * Returns true if successful.*/
    private boolean loadScripts() {
        java.net.URL detectURL = java.lang.ClassLoader.getSystemResource("yacasinit.ys");
        // if yacasinit.ys not found:
        if (detectURL == null) return false;
 
        String detect = detectURL.getPath(); // file:/home/av/src/lib/yacas.yar!/yacasinit.ys
        String archive = detect.substring(0, detect.lastIndexOf('!')); // file:/home/av/src/lib/yacas.jar
 
        java.util.zip.ZipFile z;
        try {
            z = new java.util.zip.ZipFile(new java.io.File(new java.net.URI(archive)));
        } catch (Exception e) {
            System.err.println(e.toString());
            return false;
        }
 
        // Pass the absolute path of yacas.jar to Yacas.
        LispStandard.zipFile = z;
 
        return true;
    }
 
    /** Use this method to pass an expression to the Yacas interpreter.
     *  Returns the output of the interpreter.
     */
    public String Evaluate(String input) {
        return yacas.Evaluate(input);
    }
}
