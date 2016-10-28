#ifndef YACAS_ARRAYCLASS_H
#define YACAS_ARRAYCLASS_H

#include "lispobject.h"
#include "genericobject.h"

#include <vector>

class ArrayClass final: public GenericClass
{
public:
    //required
    ArrayClass(std::size_t aSize,LispObject* aInitialItem);
    const char* TypeName() const override;

    //array-specific
    std::size_t Size() const;
    LispObject* GetElement(std::size_t aItem) const;
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
const char* ArrayClass::TypeName() const
{
    return "\"Array\"";
}

inline
std::size_t ArrayClass::Size() const
{
    return iArray.size();
}

inline
LispObject* ArrayClass::GetElement(std::size_t aItem) const
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

