/** \file lisphash.h
 *  hashing of strings. Each string will exist only once in the
 * hash table, and have an unique id.
 */


#ifndef YACAS_LISPHASH_H
#define YACAS_LISPHASH_H

#include "yacasbase.h"
#include "lispstring.h"
#include <vector>

#ifdef YACAS_NO_CONSTEXPR
const LispInt KSymTableSize = 211;
#else
constexpr LispInt KSymTableSize = 211;
#endif

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
  LispHashTable() = default;
  ~LispHashTable();
  // If string not yet in table, insert. Afterwards return the string.
  LispString * LookUp(const std::string&);
  LispString * LookUpCounted(const LispChar * aString, LispInt aLength);
  void GarbageCollect();
private:
  void AppendString(LispInt bin,LispString * result);
private:
  std::vector<LispStringSmartPtr> iHashTable[KSymTableSize];
};

inline
std::string Stringify(const std::string& s)
{
    return "\"" + s + "\"";
}

inline
LispString* LispHashTable::LookUp(const std::string& s)
{
    return LookUpCounted(s.c_str(), s.size());
}

#endif
