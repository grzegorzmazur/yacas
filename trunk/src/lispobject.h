/** \file lispobject.h
 *  Implementation of basic LispObject, which is the base class for
 *  anything that can be put in a linked list.
 *
 * class LispObject. This class implements one lisp object, which is a
 * abstract class containing reference counting and a next pointer.
 * derive from this to implement specific types of list elements.
 * The local class LispPtr implements automatic garbage collection
 * through reference counting.
 *
 */


#ifndef __lispobject_h__
#define __lispobject_h__

#include "yacasbase.h"
#include "refcount.h"
#include "lispstring.h"
#include "genericobject.h"
/*TODO remove?
#include "evalfunc.h"
*/

#ifdef YACAS_DEBUG
void IncNrObjects();
void DecNrObjects();
#endif

class LispObject;

/** class LispPtr. This class is a smart pointer type class to Lisp
 *  objects that can be inserted into linked lists. They do the actual
 *  reference counting, and consequent destruction of the object if
 *  nothing points to it. LispPtr is used in LispObject as a pointer
 *  to the next object, and in diverse parts of the built-in internal
 *  functions to hold temporary values.
 */
class LispPtr : public YacasBase
{
public:
    inline LispPtr();
    inline LispPtr(LispPtr& aOther);
    inline LispPtr(LispObject& aOther);
    inline ~LispPtr();
    LispPtr& operator=(LispPtr& aOther);
    LispPtr& operator=(LispObject& aOther);
    inline void Set(LispObject* aNext);
    inline LispObject* Get() const;
    // For use as an iterator
    inline LispObject* operator()();
    inline void GoNext();

private:
    inline void DestroyPrevious();
    inline void DoSet(LispObject* aNext);
    
private:
    LispObject* iNext;
};



/** class LispObject is the base object class that can be put in
 *  linked lists. It either has a pointer to a string, obtained through
 *  String(), or it is a holder for a sublist, obtainable through SubList(),
 *  or it is a generic object, in which case Generic() returns non-NULL.
 *  Only one of these three functions should return a non-NULL value.
 *  It is a reference-counted object. LispPtr handles the reference counting.
 */
class LispObject : public RefCountedObject
{
public:
    inline LispPtr& Next();
public: //Derivables
    virtual ~LispObject();

    /** Return string representation, or NULL if the object doesn't have one.
     *  the string representation is only relevant if the object is a
     *  simple atom. This method returns NULL by default.
     */
    virtual LispStringPtr String() const;
    /** If this object is a list, return a pointer to it.
     *  Default behaviour is to return NULL.
     */
    virtual LispPtr* SubList();
    virtual GenericClass* Generic();
/*TODO remove?
    virtual EvalFuncBase* EvalFunc();
    virtual void SetEvalFunc(EvalFuncBase* aEvalFunc);
    */
    virtual LispObject* Copy(LispInt aRecursed)=0;

    /** Return a pointer to extra info. This allows for annotating
     *  an object. Returns NULL by default.
     */
    virtual LispPtr* ExtraInfo();
    virtual LispObject* SetExtraInfo(LispPtr& aData) = 0;
public:
    LispInt Equal(LispObject& aOther);
    inline LispInt operator==(LispObject& aOther);
    inline LispInt operator!=(LispObject& aOther);
    inline void SetFlag(LispInt aFlag);
    inline void ResetFlag(LispInt aFlag);

#ifdef YACAS_DEBUG
    LispCharPtr iFileName;
    LispInt iLine;
    
    inline void SetFileAndLine(LispCharPtr aFileName, LispInt aLine)
    {
        iFileName = aFileName;
        iLine = aLine;
    }
#endif

protected:
    inline LispObject()
    {
#ifdef YACAS_DEBUG
        IncNrObjects();
#endif

#ifdef YACAS_DEBUG
        iFileName = NULL;
        iLine = 0;
#endif
    }
    
private:
    LispPtr   iNext;
};


/**
 * class LispIterator works almost like LispPtr, but doesn't enforce
 * reference counting, so it should be slightly faster. This one
 * should be used in stead of LispPtr if you are going to traverse
 * a lisp expression in a non-destructive way.
 */
class LispIterator  : public YacasBase
{
public:
    LispIterator(LispPtr& aPtr);
    LispIterator& operator=(LispPtr& aPtr);
    inline LispObject* operator()();
    inline LispPtr* Ptr();
    inline void GoNext();
    inline void GoSub();
private:
    LispPtr* iPtr;
};


#include "lispobject.inl"

#endif



