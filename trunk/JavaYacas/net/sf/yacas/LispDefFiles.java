package net.sf.yacas;

class LispDefFiles {

    LispDefFile File(String aFileName) {
        LispDefFile file = map.get(aFileName);

        if (file == null) {
            file = new LispDefFile(aFileName);
            map.put(aFileName, file);
        }

        return file;
    }

    java.util.HashMap<String, LispDefFile> map = new java.util.HashMap();
}
