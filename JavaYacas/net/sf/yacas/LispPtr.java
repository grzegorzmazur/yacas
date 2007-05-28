package net.sf.yacas;


/** class LispPtr. This class is a smart pointer type class to Lisp
 *  objects that can be inserted into linked lists. They do the actual
 *  reference counting, and consequent destruction of the object if
 *  nothing points to it. LispPtr is used in LispObject as a pointer
 *  to the next object, and in diverse parts of the built-in internal
 *  functions to hold temporary values.
 */
class LispPtr
{
    public LispPtr()
  {
    iNext = null;
  }
    public LispPtr(LispPtr aOther)
  {
    iNext = aOther.iNext;
  }
    public LispPtr(LispObject aOther)
  {
    iNext = aOther;
  }
    public void Set(LispObject aNext)
  {
    iNext = aNext;
  }
    public LispObject Get()
  {
    return iNext;
  }
    public void GoNext()
  {
    iNext = iNext.iNext.iNext;
  }
    void DoSet(LispObject aNext)
  {
    iNext = aNext;
  }
    LispObject iNext;
};
