package net.sf.yacas;


class ArrayClass extends GenericClass
{
  public ArrayClass(int aSize,LispObject aInitialItem)
  {
    iArray = new LispPtrArray(aSize,aInitialItem);
  }
  public String Send(LispArgList aArgList)
  {
    return null;
  }
  public String TypeName()
  {
    return "\"Array\"";
  }

  public int Size()
  {
    return iArray.Size();
  }
  public LispObject GetElement(int aItem) throws Exception
  {
    LispError.LISPASSERT(aItem>0 && aItem<=iArray.Size());
    return iArray.GetElement(aItem-1).Get();
  }
  public void SetElement(int aItem,LispObject aObject) throws Exception
  {
    LispError.LISPASSERT(aItem>0 && aItem<=iArray.Size());
    iArray.SetElement(aItem-1,aObject);
  }
  LispPtrArray iArray;
}
