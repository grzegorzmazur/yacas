package net.sf.yacas;

import org.junit.*;
import static org.junit.Assert.*;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import java.util.ArrayList;
import java.io.File;

@RunWith(Parameterized.class)
public class YacasTest {

    private String fname;

    public YacasTest(String fname) {
        this.fname = fname;
    }

    @Test
    public void test() {
        StringBuffer buf = new StringBuffer();

        try {
            YacasInterpreter yacas = new YacasInterpreter(new StringOutput(buf));

            yacas.Evaluate("Load(\"tests/"+fname+"\");");

            if (buf.toString().contains("******************")) {
                System.err.print(buf.toString());
                Assert.fail(buf.toString());
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
