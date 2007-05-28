package net.sf.yacas;


class LispAtom extends LispObject
{
    static LispObject New(LispEnvironment aEnvironment, String aString) throws Exception
  {
    LispObject self = null;
    if (LispStandard.IsNumber(aString,true))  // check if aString is a number (int or float)
    {
      /// construct a number from a decimal string representation (also create a number object)
      self = new LispNumber(aString, aEnvironment.Precision());
    }
    else
    {
      self = new LispAtom(aEnvironment.HashTable().LookUp(aString));
    }
    LispError.Check(self!=null,LispError.KLispErrNotEnoughMemory);
    return self;
  }
    public String String()
  {
    return iString;
  }
    public LispObject Copy(boolean aRecursed)
  {
     return new LispAtom(iString);
  }
    public LispObject SetExtraInfo(LispPtr aData)
  {
    //TODO FIXME
    System.out.println("NOT YET IMPLEMENTED!!!");
/*
    LispObject result = new LispAnnotatedObject<LispAtom>(this);
    result->SetExtraInfo(aData);
    return result;
*/
        return null;
  }
    LispAtom(String aString)
  {
    iString = aString;
  }
    String iString;
};
