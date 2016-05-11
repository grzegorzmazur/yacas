/** \file lisphash.h
 *  hashing of strings. Each string will exist only once in the
 * hash table, and have an unique id.
 */


#ifndef YACAS_LISPHASH_H
#define YACAS_LISPHASH_H

#include "lispstring.h"

#include <unordered_map>

/**
 * This is the symbol table, implemented as a hash table for fast
 * lookup. It is meant to store any string just once, have fast
 * searching for strings and return a reference to the string.
 * This also allows fast comparison of two strings (two strings
 * are equal iff the pointers to the strings are equal).
 */
class LispHashTable {
public:
    // If string not yet in table, insert. Afterwards return the string.
    const LispString* LookUp(const std::string&);
    void GarbageCollect();

private:
    std::unordered_map<std::string, LispStringSmartPtr> _rep;
};


#endif
