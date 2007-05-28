package net.sf.yacas;

/**
 * class LispIterator works almost like LispPtr, but doesn't enforce
 * reference counting, so it should be slightly faster. This one
 * should be used in stead of LispPtr if you are going to traverse
 * a lisp expression in a non-destructive way.
 */
class LispIterator
{
  public LispIterator(LispPtr aPtr)
  {
    iPtr = aPtr;
  }
  public LispObject GetObject()
  {
    return iPtr.Get();
  }
  public LispPtr Ptr()
  {
    return iPtr;
  }
  public void GoNext() throws Exception
  {
    LispError.Check(iPtr.Get() != null,LispError.KLispErrListNotLongEnough);
    iPtr = (iPtr.Get().Next());
  }
  public void GoSub() throws Exception
  {
    LispError.Check(iPtr.Get() != null,LispError.KLispErrInvalidArg);
    LispError.Check(iPtr.Get().SubList() != null,LispError.KLispErrNotList);
    iPtr = iPtr.Get().SubList();
  }
  LispPtr iPtr;
};

