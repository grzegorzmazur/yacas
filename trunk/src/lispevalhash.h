/** \file lispevalhash.h
 *  Storage of executable commands
 *
 */

#ifndef __lispevalhash_h__
#define __lispevalhash_h__

#include "yacasbase.h"
#include "lispobject.h"
#include "lisphash.h"
#include "evalfunc.h"

class LispEnvironment;
// aArguments are ALL the arguments, including the head...
typedef void (*LispEvalCaller)(LispEnvironment& aEnvironment,
                               LispPtr& aResult,
                               LispPtr& aArguments);


class LispEvaluator : public EvalFuncBase
{
public:
    LispEvaluator(LispEvalCaller aCaller): iCaller(aCaller)
    {
    }
    virtual void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
                          LispPtr& aArguments);
private:
    LispEvalCaller iCaller;
};

typedef LispAssociatedHash<LispEvaluator> LispCommands;



#endif
