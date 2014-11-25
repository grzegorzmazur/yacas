package net.sf.yacas;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.lang.InterruptedException;
import java.lang.Thread;
import java.util.ArrayList;


class StreamGobbler extends Thread {

    private InputStream is;
    private ArrayList<String> strings = new ArrayList<String>();

    public StreamGobbler(InputStream is)
    {
        this.is = is;
    }

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
};
