
#include "yacas/yacasprivate.h"
#include "yacas/arrayclass.h"

ArrayClass::ArrayClass(LispInt aSize,LispObject* aInitialItem) : iArray()
{
  iArray.resize(aSize);
  if (aInitialItem)
  {
    for (LispInt i=0;i<aSize;i++)
    {
      iArray[i] = (aInitialItem);
    }
  }
}

const LispChar * ArrayClass::TypeName()
{
    return "\"Array\"";
}
