#ifndef YACAS_ARRAYCLASS_H
#define YACAS_ARRAYCLASS_H

#include "lispobject.h"
#include "genericobject.h"

#include <vector>

class ArrayClass : public GenericClass
{
public:
    //required
    ArrayClass(std::size_t aSize,LispObject* aInitialItem);
    const LispChar* TypeName() const override;

    //array-specific
    std::size_t Size();
    LispObject* GetElement(std::size_t aItem);
    void SetElement(std::size_t aItem,LispObject* aObject);

private:
    std::vector<LispPtr> iArray;
};

inline
ArrayClass::ArrayClass(std::size_t aSize, LispObject* aInitialItem):
    iArray(aSize, LispPtr(aInitialItem))
{
}

inline
const LispChar* ArrayClass::TypeName() const
{
    return "\"Array\"";
}

inline
std::size_t ArrayClass::Size()
{
    return iArray.size();
}

inline
LispObject* ArrayClass::GetElement(std::size_t aItem)
{
    assert(aItem > 0 && aItem<=iArray.size());
    return iArray[aItem-1];
}

inline
void ArrayClass::SetElement(std::size_t aItem, LispObject* aObject)
{
    assert(aItem > 0 && aItem<=iArray.size());
    iArray[aItem-1] = aObject;
}

#endif

