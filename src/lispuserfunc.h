

#ifndef __lispuserfunc_h__
#define __lispuserfunc_h__

#include "lispobject.h"
#include "lispenvironment.h"
#include "lisphash.h"
#include "grower.h"
#include "evalfunc.h"

// class LispUserFunc : base user function api: should be able
// to evaluate with some arguments. It's a pure abstract class that
// should be obtained through an associated hash...
class LispUserFunction : public EvalFuncBase
{
public:
    LispUserFunction() : iFenced(LispTrue),iTraced(LispFalse) {};
    virtual ~LispUserFunction();
    virtual void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment,
                  LispPtr& aArguments)=0;
    virtual void HoldArgument(LispStringPtr aVariable) = 0;
    virtual void DeclareRule(LispInt aPrecedence, LispPtr& aPredicate,
                             LispPtr& aBody) = 0;
    virtual void DeclareRule(LispInt aPrecedence, LispPtr& aBody) = 0;
    virtual void DeclarePattern(LispInt aPrecedence, LispPtr& aPredicate,
                             LispPtr& aBody) = 0;
    virtual LispPtr& ArgList() = 0;

public: //unfencing
    inline void UnFence() {iFenced = LispFalse;};
    inline LispBoolean Fenced() {return iFenced;};
public: //tracing
    inline void Trace() {iTraced = LispTrue;};
    inline void UnTrace() {iTraced = LispFalse;};
    inline LispBoolean Traced() {return iTraced;};
private:
    LispBoolean iFenced;
    LispBoolean iTraced;
};

// User function with a specific arity (number of arguments)
class LispArityUserFunction : public LispUserFunction
{
public:
    virtual LispInt Arity() const = 0;
};

class LispDefFile;
class LispMultiUserFunction
{
public:
    LispMultiUserFunction() : iFileToOpen(NULL) {};
        
    LispUserFunction* UserFunc(LispInt aArity);
    virtual ~LispMultiUserFunction();
    virtual void HoldArgument(LispStringPtr aVariable);
    virtual void DefineRuleBase(LispArityUserFunction* aNewFunction);
    virtual void DeleteBase(LispInt aArity);
private:
    CDeletingArrayGrower<LispArityUserFunction*> iFunctions;
public:
    LispDefFile* iFileToOpen; // file to read for definition of this function
};


class LispUserFunctions : public LispAssociatedHash<LispMultiUserFunction>
{
};


#endif

