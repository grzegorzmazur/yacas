
#include "yacasprivate.h"
#include "arrayclass.h"

ArrayClass::ArrayClass(LispInt aSize,LispObject* aInitialItem) : iArray(aSize,aInitialItem)
{
/*TODO remove
    iSize=aSize;
    iArray = NEW LispPtr[aSize];
    LispInt i;
    for (i=0;i<aSize;i++)
    {
        iArray[i].Set(aInitialItem);
    }
*/
}


ArrayClass::~ArrayClass()
{
//printf("before delete\n");
//TODO remove    delete[] iArray;
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



