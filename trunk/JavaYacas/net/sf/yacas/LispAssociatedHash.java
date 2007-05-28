package net.sf.yacas;


/** LispAssociatedHash allows you to associate arbitrary
 * information with a string in the above hash table. You can
 * specify what type of information to link to the string, and
 * this class then stores that information for a string. It is
 * in a sense a way to extend the string object without modifying
 * the string class itself. This class does not own the strings it
 * points to, but instead relies on the fact that the strings
 * are maintained in a hash table (like LispHashTable above).
 */
class LispAssociatedHash
{
    /// Find the data associated to \a aString.
    /// If \a aString is not stored in the hash table, this function
    /// returns #NULL.
    Object LookUp(String aString)
  {
    if (iHashtable.containsKey(aString))
    return iHashtable.get(aString);
    return null;
    }

    /// Add an association to the hash table.
    /// If \a aString is already stored in the hash table, its
    /// association is changed to \a aData. Otherwise, a new
    /// association is added.
    void SetAssociation(Object aData, String aString)
  {
    if (iHashtable.containsKey(aString))
    iHashtable.remove(aString);
      iHashtable.put(aString, aData);
  }

    /// Delete an association from the hash table.
    void Release(String aString)
  {
    if (iHashtable.containsKey(aString))
    iHashtable.remove(aString);
  }
  java.util.Hashtable iHashtable = new java.util.Hashtable();
}

