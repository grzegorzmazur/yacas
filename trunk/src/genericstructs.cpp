
#include "yacasprivate.h"
#include "genericstructs.h"

GenericStruct::GenericStruct(LispChar * aTypeName, void* aData, void (*aDestructor)(void*))
:iData(aData),iTypeName(aTypeName),iDestructor(aDestructor)
{
}
GenericStruct::~GenericStruct()
{
    if (iDestructor && iData)
    iDestructor(iData);
}

LispChar * GenericStruct::Send(LispArgList& aArgList)
{
    return "";
}

LispChar * GenericStruct::TypeName()
{
    return iTypeName;
}

