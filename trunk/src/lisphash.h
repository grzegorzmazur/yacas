/** \file lisphash.h
 *  hashing of strings. Each string will exist only once in the
 * hash table, and have an unique id.
 */


#ifndef __lisphash_h__
#define __lisphash_h__

#include "grower.h"
#include "lisptype.h"
#include "lispstring.h"


const LispInt KSymTableSize = 211;
LispInt  LispHash( char *s );
LispInt  LispHashCounted( char *s,LispInt length );
LispInt  LispHashStringify( char *s );
LispInt  LispHashUnStringify( char *s );
LispInt LispHashPtr(LispStringPtr aString);

/**
 * This is the symbol table, implemented as a hash table for fast
 * lookup. It is meant to store any string just once, have fast
 * searching for strings and return a reference to the string.
 * This also allows fast comparison of two strings (two strings
 * are equal iff the pointers to the strings are equal).
 */
class LispHashTable
{
public:
    ~LispHashTable();
    // If string not yet in table, insert. Afterwards return the string.
    LispStringPtr LookUp(LispCharPtr aString,
                         LispBoolean aStringOwnedExternally=LispFalse);
    /// LookUp that takes ownership of the string
    LispStringPtr LookUp(LispStringPtr aString);
    LispStringPtr LookUpCounted(LispCharPtr aString,LispInt aLength);
    LispStringPtr LookUpStringify(LispCharPtr aString,
                         LispBoolean aStringOwnedExternally=LispFalse);
    LispStringPtr LookUpUnStringify(LispCharPtr aString,
                         LispBoolean aStringOwnedExternally=LispFalse);

    // GarbageCollect
    void GarbageCollect();
private:
    CDeletingArrayGrower<LispStringPtr> iHashTable[KSymTableSize];
};



/** VoidGrow is a helper class for LispAssociatedHash
 */
class VoidGrow : public CArrayGrower<void*>
{
};

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
class LispAssociatedHash
{
public:
    inline ~LispAssociatedHash();
    inline T* LookUp(LispStringPtr aString);
    inline void SetAssociation(const T& aData, LispStringPtr aString);
    inline void Release(LispStringPtr aString);

private:
    // The next array is in fact an array of arrays of type LAssoc<T>
    VoidGrow iHashTable[KSymTableSize];
    
};



#include "lisphash.inl"


#endif
