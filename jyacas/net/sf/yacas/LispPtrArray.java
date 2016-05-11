package net.sf.yacas;


/** \class LispPtrArray is similar to LispPtr, but implements an array
 *  of pointers to objects.
 */
class LispPtrArray
{
  public LispPtrArray(int aSize,LispObject aInitialItem)
  {
    iArray = new LispPtr[aSize];
    iSize = aSize;
    int i;
    for(i=0;i<aSize;i++)
    {
      iArray[i] = new LispPtr();
      iArray[i].Set(aInitialItem);
    }
  }
  public int Size()
  {
    return iSize;
  }
  public LispPtr GetElement(int aItem)
  {
    return iArray[aItem];
  }
  public void SetElement(int aItem,LispObject aObject)
  {
    iArray[aItem].Set(aObject);
  }
  int iSize;
  LispPtr iArray[];
}
