package net.sf.yacas;

class LispHashTable {

    String LookUp(String aString) {
        return aString.intern();
    }

    String LookUpStringify(String aString) {
        return ("\"" + aString + "\"").intern();
    }

    String LookUpUnStringify(String aString) {
        return aString.substring(1, aString.length() - 1).intern();
    }
}
