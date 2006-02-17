
#include "yacasprivate.h"
#include "arrayclass.h"

#if HAS_NEW_LispPtrArray == 0
ArrayClass::ArrayClass(LispInt aSize,LispObject* aInitialItem) : iArray(aSize,aInitialItem)
{
}
#else
ArrayClass::ArrayClass(LispInt aSize,LispObject* aInitialItem) : iArray()
{
	iArray.GrowTo(aSize);
	if (aInitialItem)
		for (LispInt i=0;i<aSize;i++)
		{
			iArray[i] = (aInitialItem);
		}
}
#endif

ArrayClass::~ArrayClass()
{
}

LispChar * ArrayClass::Send(LispArgList& aArgList)
{
    return NULL;
}
LispChar * ArrayClass::TypeName()
{
    return "\"Array\"";
}



