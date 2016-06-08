package net.sf.yacas;


class ListedMacroUserFunction extends MacroUserFunction
{

  public ListedMacroUserFunction(LispPtr  aParameters) throws Exception
  {
    super(aParameters);
  }
  @Override
  public boolean IsArity(int aArity)
  {
    return (Arity() <= aArity);
  }
  @Override
  public void Evaluate(LispPtr aResult, LispEnvironment aEnvironment, LispPtr aArguments) throws Exception
  {
    LispPtr newArgs = new LispPtr();
    LispIterator iter = new LispIterator(aArguments);
    LispPtr ptr =  newArgs;
    int arity = Arity();
    int i=0;
    while (i < arity && iter.GetObject() != null)
    {
        ptr.Set(iter.GetObject().Copy(false));
        ptr = (ptr.Get().Next());
        i++;
        iter.GoNext();
    }
    if (iter.GetObject().Next().Get() == null)
    {
        ptr.Set(iter.GetObject().Copy(false));
        iter.GoNext();
        LispError.LISPASSERT(iter.GetObject() == null);
    }
    else
    {
        LispPtr head = new LispPtr();
        head.Set(aEnvironment.iList.Copy(false));
        head.Get().Next().Set(iter.GetObject());
        ptr.Set(LispSubList.New(head.Get()));
    }
    super.Evaluate(aResult, aEnvironment, newArgs);
  }
}

