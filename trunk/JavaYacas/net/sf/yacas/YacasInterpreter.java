package net.sf.yacas;

import java.io.*;
import java.net.*;
import java.nio.file.NoSuchFileException;
import java.util.zip.*;

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
    public YacasInterpreter()  throws IOException, ZipException, URISyntaxException {
        this(new ByteArrayOutputStream());
    }

    public YacasInterpreter(OutputStream out) throws IOException, ZipException, URISyntaxException {

        String scriptsDir = "scripts/";

        yacas = new CYacas(out);

        URL initURL = java.lang.ClassLoader.getSystemResource(scriptsDir + "yacasinit.ys");

        if (initURL == null)
            throw new NoSuchFileException("yacasinit.ys not found in " + scriptsDir);

        String initPath = initURL.getPath();

        if (initPath.lastIndexOf('!') >= 0) {
            String archive = initPath.substring(0, initPath.lastIndexOf('!'));

            ZipFile z = new ZipFile(new File(new URI(archive)));

            LispStandard.zipFile = z;
        } else {
            yacas.Evaluate("DefaultDirectory(\"" + scriptsDir + "\");");
        }

        yacas.Evaluate("Load(\"yacasinit.ys\");");
    }

    /** Use this method to pass an expression to the Yacas interpreter.
     *  Returns the output of the interpreter.
     */
    public String Evaluate(String input) {
        return yacas.Evaluate(input);
    }
}
