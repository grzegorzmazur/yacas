
/*
#include <stdio.h>
#include "standard.h"
*/

#include "yacasprivate.h"
#include "lispevalhash.h"
#include "lispenvironment.h"
#include "lispatom.h"
#include "lispeval.h"
#include "errors.h"



#define InternalEval aEnvironment.iEvaluator->Eval


void YacasEvaluator::Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,LispPtr& aArguments)
{

  if (!(iFlags & Variable))
  {
    CheckNrArgs(iNrArgs+1,aArguments,aEnvironment);
  }

  LispInt stacktop = aEnvironment.iStack.GetStackTop();

  // Push a place holder for the result: push full expression so it is available for error reporting
  aEnvironment.iStack.PushArgOnStack(aArguments.Get());

  LispIterator iter(aArguments);
  iter.GoNext();

  LispInt i;
  LispInt nr = iNrArgs;

  if (iFlags & Variable) nr--;

  // Walk over all arguments, evaluating them as necessary
  if (iFlags & Macro)
  {
    for (i=0;i<nr;i++)
    {
      Check(iter() != NULL, KLispErrWrongNumberOfArgs);
      aEnvironment.iStack.PushArgOnStack(iter()->Copy(LispFalse));
      iter.GoNext();
    }
    if (iFlags & Variable)
    {
      LispPtr head;
      head.Set(LispAtom::New(aEnvironment,aEnvironment.iList));
      head.Get()->Next().Set(iter());
      aEnvironment.iStack.PushArgOnStack(LispSubList::New(head.Get()));
    }
  }
  else
  {
    LispPtr arg;
    for (i=0;i<nr;i++)
    {
      Check(iter() != NULL, KLispErrWrongNumberOfArgs);
      Check(iter.Ptr() != NULL, KLispErrWrongNumberOfArgs);
      InternalEval(aEnvironment, arg, *iter.Ptr());
      aEnvironment.iStack.PushArgOnStack(arg.Get());
      iter.GoNext();
    }
    if (iFlags & Variable)
    {

//LispString res;

//printf("Enter\n");
      LispPtr head;
      head.Set(LispAtom::New(aEnvironment,aEnvironment.iList));
      head.Get()->Next().Set(iter());
      LispPtr list;
      list.Set(LispSubList::New(head.Get()));


/*
PrintExpression(res, list,aEnvironment,100);
printf("before %s\n",res.String());
*/

      InternalEval(aEnvironment, arg, list);

/*
PrintExpression(res, arg,aEnvironment,100);
printf("after %s\n",res.String());
*/

      aEnvironment.iStack.PushArgOnStack(arg.Get());
//printf("Leave\n");
    }
  }

  iCaller(aEnvironment,stacktop);  
  aResult.Set(aEnvironment.iStack.GetElement(stacktop).Get());
  aEnvironment.iStack.PopTo(stacktop);
}


