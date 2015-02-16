package net.sf.yacas;

import java.util.HashMap;

class LispDefFiles {

    LispDefFile File(String aFileName) {
        LispDefFile file = map.get(aFileName);

        if (file == null) {
            file = new LispDefFile(aFileName);
            map.put(aFileName, file);
        }

        return file;
    }

    HashMap<String, LispDefFile> map = new HashMap<>();
}
