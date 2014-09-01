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

#endif
