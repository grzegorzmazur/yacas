/** \file lispatom.h
 *  implementation of the standard lisp elements: atom and sublist.
 *
 * class LispAtom. This class implements one atom, which is a
 * reference to a string it represents, and a pointer to the next
 * lisp atom if it is in a list.
 * The local class LispPtr implements automatic garbage collection
 * through reference counting.
 */

#ifndef __lispatom_h__
#define __lispatom_h__

#include "yacasbase.h"
#include "lispobject.h"
#include "lispstring.h"

// Flags used for atom types. These are not strictly necessary, but
// do speed up certain evaluations by avoiding a lot of overhead.
#define KFlagIsNumeric 0x01  // Quoted (after executing the args for
                          // a LispLambda, or for a LispSetQuoted)

class LispAtom : public LispObject
{
public:
    static LispObject* New(LispStringPtr aString);
    virtual ~LispAtom();
    virtual LispStringPtr String() const;
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
    virtual LispStringPtr String() const;
    virtual LispPtr* SubList();
    virtual GenericClass* Generic();
/*TODO remove?
    virtual EvalFuncBase* EvalFunc();
    virtual void SetEvalFunc(EvalFuncBase* aEvalFunc);
    */
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
LispStringPtr LispAnnotatedObject<T>::String() const
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

/*TODO remove?
template<class T>
EvalFuncBase* LispAnnotatedObject<T>::EvalFunc()
{
    return iObject.Get()->EvalFunc();
}
*/

/*TODO remove?
template<class T>
void LispAnnotatedObject<T>::SetEvalFunc(EvalFuncBase* aEvalFunc)
{
    iObject.Get()->SetEvalFunc(aEvalFunc);
}
*/
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
    virtual LispObject* Copy(LispInt aRecursed);
/*TODO remove?
    virtual EvalFuncBase* EvalFunc();
    virtual void SetEvalFunc(EvalFuncBase* aEvalFunc);
    */
public:
    virtual LispObject* SetExtraInfo(LispPtr& aData);
private:
    LispSubList(LispObject* aSubList);
private:
    LispPtr iSubList;
/*TODO remove?
    EvalFuncBase* iEvalFunc;
*/
};


class LispGenericClass : public LispObject
{
public:
    static LispGenericClass* New(GenericClass* aClass);
    virtual ~LispGenericClass();
    virtual GenericClass* Generic();
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
    LispNumber(LispHashTable* aHashTable, BigNumber* aNumber,LispStringPtr aString);
    LispNumber(LispHashTable& aHashTable, BigNumber* aNumber);
    LispNumber(LispStringPtr aString);
    virtual ~LispNumber();
    virtual LispObject* Copy(LispInt aRecursed);
    virtual LispStringPtr String() const;
    virtual BigNumber* Number(LispInt aPrecision);
    LispObject* SetExtraInfo(LispPtr& aData);
private:
    RefPtr<BigNumber> iNumber;
    LispStringPtr iString;
    LispHashTable* iHashTable;
};

#endif

#endif


