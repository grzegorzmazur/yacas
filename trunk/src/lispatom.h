/** \file lispatom.h
 *  implementation of the standard lisp elements: atom and sublist.
 *
 * class LispAtom. This class implements one atom, which is a
 * reference to a string it represents, and a pointer to the next
 * lisp atom if it is in a list.
 * It also has a pointer to the annotation object.
 * The local class LispPtr implements automatic garbage collection
 * through reference counting.
 *
 * The class LispNumber inherits from LispAtom and holds a numeric atom
 * in the string representation and in the numeric representation (BigNumber).
 * The string representation is converted to BigNumber (using the current precision for floats) when a numeric
 * operation is first requested on the atom. The BigNumber representation is
 * converted to the string representation whenever the number needs to be printed i.e. LispAtom::String() method is requested.
 * The string is held in the number (to avoid repeated conversions) and also cached in the string cache (this caching will eventually be abandoned).
 * When LispNumber is constructed from BigNumber, no string representation is available.
 * Conversion from string to BigNumber is done only if no BigNumber object is present.
 */

#ifndef __lispatom_h__
#define __lispatom_h__

#include "yacasbase.h"
#include "lispobject.h"
#include "lispstring.h"

/// This should be used whenever constants 2, 10 mean binary and decimal.
// maybe move somewhere else?
#define BASE10 10
#define BASE2 2

// Flags used for atom types. These are not strictly necessary, but
// do speed up certain evaluations by avoiding a lot of overhead.
#define KFlagIsNumeric 0x01  // Quoted (after executing the args for
                          // a LispLambda, or for a LispSetQuoted)

class LispEnvironment;

class LispAtom : public LispObject
{
public:
    static LispObject* New(LispEnvironment& aEnvironment, LispStringPtr aString);
    virtual ~LispAtom();
    virtual LispStringPtr String();
    virtual LispObject* Copy(LispInt aRecursed);
public:
    virtual LispObject* SetExtraInfo(LispPtr& aData);
private:
    LispAtom(LispStringPtr aString);
private:
    LispStringPtr iString;
};

template<class T>
class LispAnnotatedObject : public LispObject
{
public:
    LispAnnotatedObject(LispObject* aOriginal);
public:
    virtual LispStringPtr String();
    virtual LispPtr* SubList();
    virtual GenericClass* Generic();
    virtual LispObject* Copy(LispInt aRecursed);
public:
    virtual LispObject* SetExtraInfo(LispPtr& aAdditionalInfo);
    virtual LispPtr* ExtraInfo();
private:
    LispPtr iObject;
    LispPtr iAdditionalInfo;
};


template<class T>
LispAnnotatedObject<T>::LispAnnotatedObject(LispObject* aOriginal)
{
    LISPASSERT(aOriginal != NULL);
    iObject.Set(aOriginal);
}

template<class T>
LispObject* LispAnnotatedObject<T>::SetExtraInfo(LispPtr& aData)
{ iAdditionalInfo.Set(aData.Get()); return this;}

template<class T>
LispPtr* LispAnnotatedObject<T>::ExtraInfo()
{ return &iAdditionalInfo; }

template<class T>
LispStringPtr LispAnnotatedObject<T>::String()
{
    return iObject.Get()->String();
}

template<class T>
LispPtr* LispAnnotatedObject<T>::SubList()
{
    return iObject.Get()->SubList();
}

template<class T>
GenericClass* LispAnnotatedObject<T>::Generic()
{
    return iObject.Get()->Generic();
}

template<class T>
LispObject* LispAnnotatedObject<T>::Copy(LispInt aRecursed)
{
    LispPtr copied;
    copied.Set(iObject.Get()->Copy(aRecursed));
    LispObject *result = NEW LispAnnotatedObject<T>(copied.Get());
    copied.Set(iAdditionalInfo.Get()->Copy(aRecursed));
    result->SetExtraInfo(copied);
#ifdef YACAS_DEBUG
    result->SetFileAndLine(iFileName, iLine);
#endif
    return result;
}


class LispSubList : public LispObject
{
public:
    static LispSubList* New(LispObject* aSubList);
    virtual ~LispSubList();
    virtual LispPtr* SubList();
    virtual LispStringPtr String();
    virtual LispObject* Copy(LispInt aRecursed);
public:
    virtual LispObject* SetExtraInfo(LispPtr& aData);
private:
    LispSubList(LispObject* aSubList);
private:
    LispPtr iSubList;
};


class LispGenericClass : public LispObject
{
public:
    static LispGenericClass* New(GenericClass* aClass);
    virtual ~LispGenericClass();
    virtual GenericClass* Generic();
    virtual LispStringPtr String();
    virtual LispObject* Copy(LispInt aRecursed);
public:
    virtual LispObject* SetExtraInfo(LispPtr& aData);
private:
    LispGenericClass(GenericClass* aClass);
private:
    GenericClass* iClass;
};

#ifndef NO_USE_BIGFLOAT
class LispHashTable;
class LispNumber : public LispObject
{
public:
    /// constructors:
    /// construct from another LispNumber
    LispNumber(LispHashTable* aHashTable, BigNumber* aNumber,LispStringPtr aString);
    /// construct from a BigNumber; the string representation will be absent
    LispNumber(LispHashTable& aHashTable, BigNumber* aNumber);
    /// construct from a decimal string representation (also create a number object) and use aPrecision bits (not decimal digits!) 
    LispNumber(LispHashTable& aHashTable, LispStringPtr aString, LispInt aPrecision);
    virtual ~LispNumber();
    virtual LispObject* Copy(LispInt aRecursed);
    /// return a string representation in decimal with maximum decimal precision allowed by the inherent accuracy of the number
    virtual LispStringPtr String();
    /// give access to the BigNumber object; if necessary, will create a BigNumber object out of the stored string, at given precision (in decimal?)
    virtual BigNumber* Number(LispInt aPrecision);
    /// annotate
    LispObject* SetExtraInfo(LispPtr& aData);
private:
    /// number object; NULL if not yet converted from string
    RefPtr<BigNumber> iNumber;
    /// string representation in decimal; NULL if not yet converted from BigNumber
    RefPtr<LispString> iString;
    /// hash table pointer (for saving strings)
    LispHashTable* iHashTable;
};

#endif

#endif


