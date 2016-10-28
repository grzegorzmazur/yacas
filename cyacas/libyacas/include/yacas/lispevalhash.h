/** \file lispevalhash.h
 *  Storage of executable commands
 *
 */

#ifndef YACAS_LISPEVALHASH_H
#define YACAS_LISPEVALHASH_H

#include "lispobject.h"
#include "evalfunc.h"


#include <unordered_map>

// new-style evaluator, passing arguments onto the stack in LispEnvironment
typedef void (*YacasEvalCaller)(LispEnvironment& aEnvironment,int aStackTop);
class YacasEvaluator: public EvalFuncBase
{
public:
  // FunctionFlags can be orred when passed to the constructor of this function
  enum FunctionFlags
  {
    Function=0,   // Function: evaluate arguments
    Macro=1,      // Function: don't evaluate arguments
    Fixed = 0,    // fixed number of arguments
    Variable = 2  // variable number of arguments
  };
  YacasEvaluator(YacasEvalCaller aCaller,int aNrArgs, int aFlags)
    : iCaller(aCaller), iNrArgs(aNrArgs), iFlags(aFlags)
  {
  }
  void Evaluate(LispPtr& aResult,
                LispEnvironment& aEnvironment,
                LispPtr& aArguments) const override;
private:
  YacasEvalCaller iCaller;
  int iNrArgs;
  int iFlags;
};

typedef std::unordered_map<LispStringSmartPtr, YacasEvaluator, std::hash<const LispString*> > YacasCoreCommands;


#endif
