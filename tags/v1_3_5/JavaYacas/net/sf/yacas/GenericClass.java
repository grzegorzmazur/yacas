package net.sf.yacas;


/// Abstract class which can be put inside a LispGenericClass.
abstract class GenericClass
{
    public  GenericClass()
  {
  }
    public abstract String Send(LispArgList aArgList);
    public abstract String TypeName();
}

