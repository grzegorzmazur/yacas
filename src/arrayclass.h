
#ifndef __arrayclass_h__
#define __arrayclass_h__

#include "yacasbase.h"
#include "lispobject.h"
#include "genericobject.h"

class ArrayClass : public GenericClass
{
public: //required
    ArrayClass(LispInt aSize,LispObject* aInitialItem);
    virtual ~ArrayClass();
    virtual LispCharPtr Send(LispArgList& aArgList);
    virtual LispCharPtr TypeName();
public: //array-specific
    inline LispInt Size();
    inline LispObject* GetElement(LispInt aItem);
    inline void SetElement(LispInt aItem,LispObject* aObject);

private:
    LispInt iSize;
    LispPtr* iArray;
};

#include "arrayclass.inl"
#endif

