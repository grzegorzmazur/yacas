
/* This file was automatically generated with compilecpp.
*/

//#include <stdio.h>

#include "lisptype.h"
#include "lispenvironment.h"
#include "lispatom.h"
#include "standard.h"
#include "arggetter.h"
#include "lispplugin.h"
#include "platmath.h"
#include "stubs.h"
#include "genericstructs.h"
#include "mathcommands.h"


#define ATOM(_x) LispAtom::New(aEnvironment,_x)
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)
#define PUSH(_x)   aEnvironment.iStack.PushArgOnStack(_x)
#define POP(_i)    aEnvironment.iStack.PopTo(aEnvironment.iStack.GetStackTop() - _i)
#define STACKTOP() aEnvironment.iStack.GetStackTop()
#define STACK(_i)  aEnvironment.iStack.GetElement(_i)

#define ISTRUE(_x) IsTrue(aEnvironment, _x)
#define ISFALSE(_x) IsFalse(aEnvironment, _x)


void Compiled_Trigonometry(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  PUSH(ATOM("True"));
  POP(1);
/* Local(x2,orig)  */
LispPtr x2 ; x2 .Set(ATOM("x2"));
LispPtr orig ; orig .Set(ATOM("orig"));
  PUSH(ATOM("True"));
  POP(1);
/* Set(x2,MathMultiply(x,x))  */
  PUSH(NULL);
  {
/* local : x  */
  PUSH(ARGUMENT(1).Get());
/* local : x  */
  PUSH(ARGUMENT(1).Get());
    LispMultiply(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  x2.Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* While (Equals(Equals(term,0),False) ) */
   for(;;)
{
  PUSH(NULL);
  {
  PUSH(NULL);
  {
/* local : term  */
  PUSH(ARGUMENT(4).Get());
/* number : 0  */
  PUSH(ATOM("0"));
    LispEquals(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
/* atom : False  */
  PUSH(ATOM("False"));
    LispEquals(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
   if (ISFALSE(STACK(STACKTOP()-1))) break;
  PUSH(ATOM("True"));
  POP(1);
/* Set(term,MathMultiply(term,x2))  */
  PUSH(NULL);
  {
/* local : term  */
  PUSH(ARGUMENT(4).Get());
/* local : x2  */
  PUSH(x2.Get());
    LispMultiply(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  ARGUMENT(4).Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* Set(i,MathAdd(i,1.0))  */
  PUSH(NULL);
  {
/* local : i  */
  PUSH(ARGUMENT(2).Get());
/* number : 1.0  */
  PUSH(ATOM("1.0"));
    LispAdd(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  ARGUMENT(2).Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* Set(term,MathDivide(term,i))  */
  PUSH(NULL);
  {
/* local : term  */
  PUSH(ARGUMENT(4).Get());
/* local : i  */
  PUSH(ARGUMENT(2).Get());
    LispDivide(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  ARGUMENT(4).Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* Set(i,MathAdd(i,1.0))  */
  PUSH(NULL);
  {
/* local : i  */
  PUSH(ARGUMENT(2).Get());
/* number : 1.0  */
  PUSH(ATOM("1.0"));
    LispAdd(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  ARGUMENT(2).Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* Set(term,MathDivide(MathSubtract(0,term),i))  */
  PUSH(NULL);
  {
  PUSH(NULL);
  {
/* number : 0  */
  PUSH(ATOM("0"));
/* local : term  */
  PUSH(ARGUMENT(4).Get());
    LispSubtract(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
/* local : i  */
  PUSH(ARGUMENT(2).Get());
    LispDivide(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  ARGUMENT(4).Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* Set(sum,MathAdd(sum,term))  */
  PUSH(NULL);
  {
/* local : sum  */
  PUSH(ARGUMENT(3).Get());
/* local : term  */
  PUSH(ARGUMENT(4).Get());
    LispAdd(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  ARGUMENT(3).Set(STACK(STACKTOP()-1).Get());
  POP(2);
}
  POP(1);
/* local : sum  */
  PUSH(ARGUMENT(3).Get());
  RESULT.Set(STACK(STACKTOP()-1).Get());
  POP(1);
}
void Compiled_MySin(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  PUSH(NULL);
  {
/* local : x  */
  PUSH(ARGUMENT(1).Get());
/* number : 1.0  */
  PUSH(ATOM("1.0"));
/* local : x  */
  PUSH(ARGUMENT(1).Get());
/* local : x  */
  PUSH(ARGUMENT(1).Get());
    Compiled_Trigonometry(aEnvironment,STACKTOP()-5 );
    POP(4 );
  }
  RESULT.Set(STACK(STACKTOP()-1).Get());
  POP(1);
}
void Compiled_MyCos(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  PUSH(NULL);
  {
/* local : x  */
  PUSH(ARGUMENT(1).Get());
/* number : 0.0  */
  PUSH(ATOM("0.0"));
/* number : 1.0  */
  PUSH(ATOM("1.0"));
/* number : 1.0  */
  PUSH(ATOM("1.0"));
    Compiled_Trigonometry(aEnvironment,STACKTOP()-5 );
    POP(4 );
  }
  RESULT.Set(STACK(STACKTOP()-1).Get());
  POP(1);
}
void Compiled_MyArcSin(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  PUSH(ATOM("True"));
  POP(1);
/* Local(result)  */
LispPtr result ; result .Set(ATOM("result"));
  PUSH(ATOM("True"));
  POP(1);
/* Set(result,FastArcSin(int1))  */
  PUSH(NULL);
  {
/* local : int1  */
  PUSH(ARGUMENT(1).Get());
    LispFastArcSin(aEnvironment,STACKTOP()-2 );
    POP(1 );
  }
  result.Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* Local(x,q,s,c)  */
LispPtr x ; x .Set(ATOM("x"));
LispPtr q ; q .Set(ATOM("q"));
LispPtr s ; s .Set(ATOM("s"));
LispPtr c ; c .Set(ATOM("c"));
  PUSH(ATOM("True"));
  POP(1);
/* Set(q,MathSubtract(MySin(result),int1))  */
  PUSH(NULL);
  {
  PUSH(NULL);
  {
/* local : result  */
  PUSH(result.Get());
    Compiled_MySin(aEnvironment,STACKTOP()-2 );
    POP(1 );
  }
/* local : int1  */
  PUSH(ARGUMENT(1).Get());
    LispSubtract(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  q.Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* While (Equals(Equals(q,0),False) ) */
   for(;;)
{
  PUSH(NULL);
  {
  PUSH(NULL);
  {
/* local : q  */
  PUSH(q.Get());
/* number : 0  */
  PUSH(ATOM("0"));
    LispEquals(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
/* atom : False  */
  PUSH(ATOM("False"));
    LispEquals(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
   if (ISFALSE(STACK(STACKTOP()-1))) break;
  PUSH(ATOM("True"));
  POP(1);
/* Set(s,MathSubtract(int1,MySin(result)))  */
  PUSH(NULL);
  {
/* local : int1  */
  PUSH(ARGUMENT(1).Get());
  PUSH(NULL);
  {
/* local : result  */
  PUSH(result.Get());
    Compiled_MySin(aEnvironment,STACKTOP()-2 );
    POP(1 );
  }
    LispSubtract(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  s.Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* Set(c,MyCos(result))  */
  PUSH(NULL);
  {
/* local : result  */
  PUSH(result.Get());
    Compiled_MyCos(aEnvironment,STACKTOP()-2 );
    POP(1 );
  }
  c.Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* Set(q,MathDivide(s,c))  */
  PUSH(NULL);
  {
/* local : s  */
  PUSH(s.Get());
/* local : c  */
  PUSH(c.Get());
    LispDivide(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  q.Set(STACK(STACKTOP()-1).Get());
  POP(1);
/* Set(result,MathAdd(result,q))  */
  PUSH(NULL);
  {
/* local : result  */
  PUSH(result.Get());
/* local : q  */
  PUSH(q.Get());
    LispAdd(aEnvironment,STACKTOP()-3 );
    POP(2 );
  }
  result.Set(STACK(STACKTOP()-1).Get());
  POP(2);
}
  POP(1);
/* local : result  */
  PUSH(result.Get());
  RESULT.Set(STACK(STACKTOP()-1).Get());
  POP(1);
}




class Plugin_test : public LispPluginBase
{
public:
    virtual void Add(LispEnvironment& aEnvironment);
    virtual void Remove(LispEnvironment& aEnvironment);
};

void Plugin_test::Add(LispEnvironment& aEnvironment)
{
  aEnvironment.SetCommand(Compiled_MyArcSin,"MyArcSin",1 ,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(Compiled_MyCos,"MyCos",1 ,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(Compiled_MySin,"MySin",1 ,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(Compiled_Trigonometry,"Trigonometry",4 ,YacasEvaluator::Function | YacasEvaluator::Fixed);

}
void Plugin_test::Remove(LispEnvironment& aEnvironment)
{
aEnvironment.RemoveCoreCommand("MyArcSin");
aEnvironment.RemoveCoreCommand("MyCos");
aEnvironment.RemoveCoreCommand("MySin");
aEnvironment.RemoveCoreCommand("Trigonometry");

}


extern "C" {
LispPluginBase* maker(void)
{
    return NEW Plugin_test;
}
};
