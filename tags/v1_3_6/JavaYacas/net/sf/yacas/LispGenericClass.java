package net.sf.yacas;


class LispGenericClass extends LispObject
{

    public static LispGenericClass New(GenericClass aClass) throws Exception
    {
      LispError.LISPASSERT(aClass!=null);
      LispGenericClass self = new LispGenericClass(aClass);
      LispError.Check(self!=null,LispError.KLispErrNotEnoughMemory);
      return self;
    }
    public GenericClass Generic()
    {
      return iClass;
    }
    public String String()
    {
      return null;
    }
    public LispObject Copy(boolean aRecursed)
    {
      LispObject copied = new LispGenericClass(iClass);
      return copied;
    }
    public LispObject SetExtraInfo(LispPtr aData)
    {
      //TODO FIXME
      return null;
    }

    LispGenericClass(GenericClass aClass)
    {
      iClass = aClass;
    }
    GenericClass iClass;
};
