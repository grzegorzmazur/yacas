
#include "yacasprivate.h"
#include "arrayclass.h"

ArrayClass::ArrayClass(LispInt aSize,LispObject* aInitialItem)
{
    iSize=aSize;
    iArray = new LispPtr[aSize];
    LispInt i;
    for (i=0;i<aSize;i++)
    {
        iArray[i].Set(aInitialItem);
    }
}


ArrayClass::~ArrayClass()
{
//printf("before delete\n");
    delete[] iArray;
//printf("after delete\n");
}

LispCharPtr ArrayClass::Send(LispArgList& aArgList)
{
    return NULL;
}
LispCharPtr ArrayClass::TypeName()
{
    return "\"Array\"";
}



