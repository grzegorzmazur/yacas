package net.sf.yacas;


abstract class YacasEvalCaller
{
  public abstract void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception;

  public static LispPtr RESULT(LispEnvironment aEnvironment,int aStackTop) throws Exception
  {
    return aEnvironment.iStack.GetElement(aStackTop);
  }
  public static LispPtr ARGUMENT(LispEnvironment aEnvironment,int aStackTop, int i)  throws Exception
  {
    return aEnvironment.iStack.GetElement(aStackTop+i);
  }

  public static LispPtr Argument(LispPtr cur, int n) throws Exception
  {
      LispError.LISPASSERT(n>=0);

      LispPtr loop = cur;
      while(n != 0)
      {
        n--;
        loop = loop.Get().Next();
      }
      return loop;
  }

}
