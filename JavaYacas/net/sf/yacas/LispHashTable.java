package net.sf.yacas;

class LispHashTable
{
    // If string not yet in table, insert. Afterwards return the string.
    String LookUp(String aString)
  {
    if (!iHashtable.containsKey(aString))
      iHashtable.put(aString,aString);
    return (String)iHashtable.get(aString);
  }
    String LookUpStringify(String aString)
  {
    aString = "\""+aString+"\"";
    if (!iHashtable.containsKey(aString))
      iHashtable.put(aString,aString);
    return (String)iHashtable.get(aString);
  }
    String LookUpUnStringify(String aString)
  {
    aString = aString.substring(1,aString.length()-1);
    if (!iHashtable.containsKey(aString))
      iHashtable.put(aString,aString);
    return (String)iHashtable.get(aString);
  }

    // GarbageCollect
    void GarbageCollect()
  {
    //TODO FIXME
  }
  java.util.Hashtable iHashtable = new java.util.Hashtable();
}
