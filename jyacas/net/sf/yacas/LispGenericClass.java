package net.sf.yacas;


class LispGenericClass extends LispObject
{

    public static LispGenericClass New(GenericClass aClass) throws Exception
    {
      LispError.LISPASSERT(aClass!=null);
      return new LispGenericClass(aClass);
    }
    @Override
    public GenericClass Generic()
    {
      return iClass;
    }
    @Override
    public String String()
    {
      return null;
    }
    @Override
    public LispObject Copy(boolean aRecursed)
    {
      LispObject copied = new LispGenericClass(iClass);
      return copied;
    }
    LispGenericClass(GenericClass aClass)
    {
      iClass = aClass;
    }
    GenericClass iClass;
}