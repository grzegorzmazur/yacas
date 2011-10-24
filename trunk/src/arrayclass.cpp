
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

const LispChar * ArrayClass::Send(LispArgList& aArgList)
{
    return NULL;
}
const LispChar * ArrayClass::TypeName()
{
    return "\"Array\"";
}



