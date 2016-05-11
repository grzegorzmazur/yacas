package net.sf.yacas;


class LispSubList extends LispObject
{
  public static LispSubList New(LispObject aSubList)
  {
    return new LispSubList(aSubList);
  }
  @Override
    public LispPtr SubList()
  {
    return iSubList;
  }
  @Override
  public String String()
  {
    return null;
  }
  @Override
  public LispObject Copy(boolean aRecursed) throws Exception
  {
    //TODO recursed copy needs to be implemented still
    LispError.LISPASSERT(aRecursed == false);
    LispObject copied = new LispSubList(iSubList.Get());
    return copied;
  }
  @Override
    public LispObject SetExtraInfo(LispPtr aData)
  {
//TODO FIXME
/*
    LispObject* result = NEW LispAnnotatedObject<LispSubList>(this);
    result->SetExtraInfo(aData);
    return result;
*/
return null;
  }
    LispSubList(LispObject aSubList)
  {
    iSubList.Set(aSubList);
  }
    LispPtr iSubList = new LispPtr();
};
