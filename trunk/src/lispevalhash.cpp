

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
  aEnvironment.iStack.PushArgOnStack(aArguments);

  LispIterator iter(aArguments);
  ++iter;

  LispInt i;
  LispInt nr = iNrArgs;

  if (iFlags & Variable) nr--;

  // Walk over all arguments, evaluating them as necessary
  if (iFlags & Macro)
  {
    for (i=0;i<nr;i++)
    {
      Check(iter.getObj(), KLispErrWrongNumberOfArgs);
      aEnvironment.iStack.PushArgOnStack(iter.getObj()->Copy());
      ++iter;
    }
    if (iFlags & Variable)
    {
      LispPtr head(aEnvironment.iList->Copy());
      head->Nixed() = (iter.getObj());
      aEnvironment.iStack.PushArgOnStack(LispSubList::New(head));
    }
  }
  else
  {
    LispPtr arg;
    for (i=0;i<nr;i++)
    {
      Check(iter.getObj(), KLispErrWrongNumberOfArgs);
      InternalEval(aEnvironment, arg, *iter);
      aEnvironment.iStack.PushArgOnStack(arg);
      ++iter;
    }
    if (iFlags & Variable)
    {

//LispString res;

//printf("Enter\n");
      LispPtr head(aEnvironment.iList->Copy());
      head->Nixed() = (iter.getObj());
      LispPtr list(LispSubList::New(head));


/*
PrintExpression(res, list,aEnvironment,100);
printf("before %s\n",res.String());
*/

      InternalEval(aEnvironment, arg, list);

/*
PrintExpression(res, arg,aEnvironment,100);
printf("after %s\n",res.String());
*/

      aEnvironment.iStack.PushArgOnStack(arg);
//printf("Leave\n");
    }
  }

  iCaller(aEnvironment,stacktop);
  aResult = (aEnvironment.iStack.GetElement(stacktop));
  aEnvironment.iStack.PopTo(stacktop);
}


