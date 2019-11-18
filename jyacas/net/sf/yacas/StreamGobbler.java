package net.sf.yacas;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;


class StreamGobbler extends Thread {

    private final InputStream is;
    private final ArrayList<String> strings;

    public StreamGobbler(InputStream is)
    {
        this.is = is;
        this.strings = new ArrayList<>();
    }

    @Override
    public void run()
    {
        InputStreamReader isr = new InputStreamReader(is);
        BufferedReader br = new BufferedReader(isr);
        String line=null;
        try {
            while ((line = br.readLine()) != null)
                strings.add(line);
        } catch (IOException e) {
        }
    }

    public ArrayList<String> shutdown()
    {
        try {
            if (isAlive())
                interrupt();
        } finally {
            return strings;
        }
    }
}
