
#include "yacasprivate.h"
#include "arrayclass.h"

ArrayClass::ArrayClass(LispInt aSize,LispObject* aInitialItem) : iArray(aSize,aInitialItem)
{
}


ArrayClass::~ArrayClass()
{
}

LispCharPtr ArrayClass::Send(LispArgList& aArgList)
{
    return NULL;
}
LispCharPtr ArrayClass::TypeName()
{
    return "\"Array\"";
}



