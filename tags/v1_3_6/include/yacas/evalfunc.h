#ifndef YACAS_EVALFUNCBASE_H
#define YACAS_EVALFUNCBASE_H

#include "yacasbase.h"

// class EvalFuncBase defines the interface to 'something that can
// evaluate'
class LispEnvironment;
class EvalFuncBase
{
public:
    virtual void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
                  LispPtr& aArguments)=0;
    virtual ~EvalFuncBase() = default;
};

#endif
