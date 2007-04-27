//TODO should we just remove this version of te compiler?

#include "lispasem.h"
#include "lispio.h"
#include "infixparser.h"

LispPtr stack[1000];
LispPtr *stackCurrent = stack;
LispNativeFunctions native;


LispObject* MakeAtom(LispEnvironment& aEnvironment, char* s)
{
  return ATOML(s);
}

LispObject* MakeAtomInt(LispEnvironment& aEnvironment, int i)
{
  LispChar s[30];
  InternalIntToAscii(s, i);
  return ATOML(s);
}

LispInt MakeGetInteger(LispPtr& aVal)
{
  return InternalAsciiToInt(aVal->String()->c_str());
}

LispObject* MakeList(LispObject* obj)
{
  return LIST(obj);
}
LispObject* AtomAdd(LispObject* obj1, LispObject* obj2)
{
  obj1->Next() = (obj2);
  return obj1;
}

void MakeLocal(LispEnvironment& aEnvironment, char* var)
{
  aEnvironment.NewLocal(aEnvironment.HashTable().LookUp(var),NULL);
}

void MakePop()
{
  stackCurrent--;
  (*stackCurrent) = (NULL);
}
void MakeEval(LispEnvironment& aEnvironment,LispPtr& aTrg, LispPtr& aSrc)
{
  LispPtr r;
  aEnvironment.iEvaluator->Eval(aEnvironment,r,aSrc);
  aTrg = (r);
}

LispInt MakePushArguments(LispEnvironment& aEnvironment, LispPtr& aArguments)
{
  LispInt nr=0;

  LispPtr* loop = &aArguments;
  loop = &(*loop)->Next();
  
  while((*loop))
  {
    PUSH();
    nr++;
    SET(TOP(0) ,Argument(aArguments,nr));
    loop = &(*loop)->Next();
  }
  return nr;
}

void MakeWrite(LispEnvironment& aEnvironment, LispPtr& aObject)
{
  aEnvironment.CurrentPrinter().Print(aObject,
                                      *aEnvironment.CurrentOutput(),
                                      aEnvironment);
}

void DebugShowStack(char* str, LispEnvironment& aEnvironment)
{
  printf("SHOWSTACK %s\n",str);
  LispPtr* ptr = stack;
  LispInt i=0;
  while (ptr<=stackCurrent)
  {
    printf("%d: ",i);
    if ((*ptr))
        MakeWrite(aEnvironment, *ptr);
    printf("\n");
    ptr++;
    i++;
  }
}

