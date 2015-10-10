
#include "yacas/yacasprivate.h"
#include "yacas/genericstructs.h"

GenericStruct::GenericStruct(LispChar * aTypeName, void* aData, void (*aDestructor)(void*))
:iData(aData),iTypeName(aTypeName),iDestructor(aDestructor)
{
}
GenericStruct::~GenericStruct()
{
    if (iDestructor && iData)
    iDestructor(iData);
}

const LispChar * GenericStruct::TypeName() const
{
    return iTypeName;
}

