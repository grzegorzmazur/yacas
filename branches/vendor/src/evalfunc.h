
#ifndef __evalfuncbase_h__
#define __evalfuncbase_h__

// class EvalFuncBase defines the interface to 'something that can
// evaluate'
class LispPtr;
class LispEnvironment;
class EvalFuncBase
{
public:
    virtual void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
                  LispPtr& aArguments)=0;
};

#endif
