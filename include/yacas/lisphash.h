/** \file lisphash.h
 *  hashing of strings. Each string will exist only once in the
 * hash table, and have an unique id.
 */


#ifndef YACAS_LISPHASH_H
#define YACAS_LISPHASH_H

#include "yacasbase.h"
#include "lispstring.h"
#include <vector>

const LispInt KSymTableSize = 211;
LispInt LispHash( const char *s );
LispInt LispHashCounted( const char *s, LispInt length );
LispInt LispHashStringify( const char *s );
LispInt LispHashPtr(const LispString * aString);  // hash the *address*!

/**
 * This is the symbol table, implemented as a hash table for fast
 * lookup. It is meant to store any string just once, have fast
 * searching for strings and return a reference to the string.
 * This also allows fast comparison of two strings (two strings
 * are equal iff the pointers to the strings are equal).
 */
class LispHashTable : public YacasBase
{
public:
  LispHashTable() {}
  ~LispHashTable();
  // If string not yet in table, insert. Afterwards return the string.
  /// LookUp that takes ownership of the string
  LispString * LookUp(const LispChar * aString);
  LispString * LookUpCounted(const LispChar * aString, LispInt aLength);
  LispString * LookUpStringify(const LispChar * aString);
  void GarbageCollect();
private:
  void AppendString(LispInt bin,LispString * result);
private:
  std::vector<LispStringSmartPtr> iHashTable[KSymTableSize];
};

inline
LispString* LispHashTable::LookUp(const LispChar* aString)
{
    return LookUpCounted(aString, std::strlen(aString));
}


/** LispAssociatedHash allows you to associate arbitrary
 * information with a string in the above hash table. You can
 * specify what type of information to link to the string, and
 * this class then stores that information for a string. It is
 * in a sense a way to extend the string object without modifying
 * the string class itself. This class does not own the strings it
 * points to, but instead relies on the fact that the strings
 * are maintained in a hash table (like LispHashTable above).
 */
template<class T>
class LispAssociatedHash : public YacasBase
{
public:
  /// Find the data associated to \a aString.
  /// If \a aString is not stored in the hash table, this function
  /// returns #nullptr.
  inline T* LookUp(LispString * aString);

  /// Add an association to the hash table.
  /// If \a aString is already stored in the hash table, its
  /// association is changed to \a aData. Otherwise, a new
  /// association is added.
  inline void SetAssociation(const T& aData, LispString * aString);

  /// Delete an association from the hash table.
  inline void Release(LispString * aString);

protected:
  /** The destructor is made protected because we do not want the outside world to directly
   *  call this destructor. The alternative would be to make the destructor virtual.
   */
  inline ~LispAssociatedHash();

private:
  // The next array is in fact an array of arrays of type LAssoc<T>
  std::vector<void*> iHashTable[KSymTableSize];
};



#include "lisphash.inl"


#endif