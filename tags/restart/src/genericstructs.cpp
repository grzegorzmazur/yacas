
#include "genericstructs.h"

GenericStruct::GenericStruct(LispCharPtr aTypeName, void* aData, void (*aDestructor)(void*))
:iTypeName(aTypeName),iData(aData),iDestructor(aDestructor)
{
}
GenericStruct::~GenericStruct()
{
    if (iDestructor != NULL && iData != NULL)
    iDestructor(iData);
}

LispCharPtr GenericStruct::Send(LispArgList& aArgList)
{
    return "";
}

LispCharPtr GenericStruct::TypeName()
{
    return iTypeName;
}

