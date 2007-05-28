
#include "yacasprivate.h"
#include "arrayclass.h"

ArrayClass::ArrayClass(LispInt aSize,LispObject* aInitialItem) : iArray()
{
  iArray.ResizeTo(aSize);
  if (aInitialItem)
  {
    for (LispInt i=0;i<aSize;i++)
    {
      iArray[i] = (aInitialItem);
    }
  }
}


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



