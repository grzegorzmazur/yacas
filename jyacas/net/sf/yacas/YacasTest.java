package net.sf.yacas;

import org.junit.*;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import java.util.ArrayList;
import java.io.File;
import java.io.StringWriter;

@RunWith(Parameterized.class)
public class YacasTest {

    private final String fname;

    public YacasTest(String fname) {
        this.fname = fname;
    }

    @Test
    public void test() {
        StringWriter output = new StringWriter();

        try {
            YacasInterpreter yacas = new YacasInterpreter(output);

            yacas.Evaluate("Load(\"tests/"+fname+"\");");

            if (output.toString().contains("******************")) {
                System.err.print(output.toString());
                Assert.fail(output.toString());
            }
        } catch (Exception e) {
            System.err.print("Error: " + e.getMessage());
            Assert.fail(e.getMessage());
        }
    }

    @Parameters
    public static ArrayList<String[]> data() {

        ArrayList<String[]> data = new ArrayList<>();

        File tests_dir = new File("tests");

        File[] files = tests_dir.listFiles();

        for (File f: files) {
            if (f.isFile()) {
                String fname = f.getName();
                if (fname.endsWith(".yts")) {
                    String[] a = {fname};
                    data.add(a);
                }
            }
        }

        return data;
    }
}
