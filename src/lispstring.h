/** \file lispstring.h
 *  Defining a string class.
 */


#ifndef __lispstring_h__
#define __lispstring_h__

#include "yacasbase.h"
#include "grower.h"
#include "refcount.h"

class LispStringSmartPtr;


/** \class LispString : zero-terminated byte-counted string.
 * Also keeps a reference count for any one interested.
 * LispString is derived from CArrayGrower, so the function
 * NrItems returns the length of the buffer. Since the string
 * is also zero-terminated (for better interfacing with the normal
 * c functions), the string length is NrItems()-1.
 *
 * This class also allows the string to point to a buffer which is owned
 * by another part of the system, in which case it cannot be resized.
 * The array will then not be freed by this class.
 */
class LispString : public CArrayGrower<LispChar>, public RefCountedObjectBase
{
public:
    /** Constructor which allows the caller to specify whether
     * the buffer is owned externally. Use the assignment operator
     * to set the string after this.
     */
    inline LispString(LispBoolean aStringOwnedExternally=LispFalse);

    /// Construct from another string
    inline LispString(LispString &aString,
                      LispBoolean aStringOwnedExternally=LispFalse);
    /** Set string from assignment. The assignment abides by earlier
     * functions setting the string as owned externally.
     */
    inline LispString& operator=(LispCharPtr aString);
    /// Construct from string.
    inline LispString(LispCharPtr aString,
                      LispBoolean aStringOwnedExternally=LispFalse);
    /// String() returns the pointer to the first character.
    inline LispCharPtr String() const;
    /** String comparison. If the string is in the hash table it is faster
     * to compare the pointers to the strings, since in that case if they
     * are equal they should in fact be literally the same object.
     */
    LispInt operator==(const LispString& aString);
    
    /** Set string by taking part of another string. The string cannot
     * be owned externally for this operation
     */
    void SetStringCounted(LispCharPtr aString,LispInt aLength);
    /** Set string from other string, adding quotes around the string.
     * The string cannot be owned externally for this operation
     */
    void SetStringUnStringified(LispCharPtr aString);
    /** Set string from other string, removing quotes around the string.
     * The string cannot be owned externally for this operation
     */
    void SetStringStringified(LispCharPtr aString);

    ~LispString();
private:
    void SetString(LispCharPtr aString,
                   LispBoolean aStringOwnedExternally=LispFalse);

};

#define LispStringRef LispString &
#define LispStringPtr LispString *

/** \class LispStringSmartPtr for managing strings outside
 of normal objects. This is the equivalent of LispPtr, maintaining
 a reference count for the string object.
 */
class LispStringSmartPtr
{
public:
    LispStringSmartPtr():iString(NULL){};
    ~LispStringSmartPtr();
    inline LispStringPtr operator() () const {return iString;}
    void Set(LispStringPtr aString);
    //TODO remove! Ugly hack! (or at least, make accessible only from LispHash)
    inline void SetInitial(LispStringPtr aString) {iString=NULL;Set(aString);};
private:
    LispStringSmartPtr(const LispStringSmartPtr& aOther){Set(aOther());};
    // do not allow this type of copying!
    LispStringPtr iString;
};

#include "lispstring.inl"
#endif


