#ifndef YACAS_ARRAYCLASS_H
#define YACAS_ARRAYCLASS_H

#include "yacasbase.h"
#include "lispobject.h"
#include "genericobject.h"

class ArrayClass : public GenericClass
{
public: //required
    ArrayClass(LispInt aSize,LispObject* aInitialItem);
    virtual ~ArrayClass();
    virtual const LispChar * TypeName();
public: //array-specific
    inline LispInt Size();
    inline LispObject* GetElement(LispInt aItem); // TODO: 1-based, ...
    inline void SetElement(LispInt aItem,LispObject* aObject);

private:
    LispPtrArray iArray;
};

#include "arrayclass.inl"
#endif

