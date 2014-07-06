package net.sf.yacas;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.lang.InterruptedException;
import java.lang.Thread;
import java.util.ArrayList;


class StreamGobbler extends Thread {

    private boolean shutdown_requested;
    private InputStream is;
    private ArrayList<String> strings = new ArrayList<String>();

    public StreamGobbler(InputStream is)
    {
        this.shutdown_requested = false;
        this.is = is;
    }

    public void run()
    {
        InputStreamReader isr = new InputStreamReader(is);
        BufferedReader br = new BufferedReader(isr);
        String line=null;
        try {
            while (!shutdown_requested && (line = br.readLine()) != null)
                strings.add(line);
        } catch (IOException e) {
        }
    }

    public ArrayList<String> shutdown()
    {
        shutdown_requested = true;
        try {
            join(50);
            if (isAlive())
                interrupt();
        } catch (InterruptedException e) {
        }
        return strings;
    }
};
