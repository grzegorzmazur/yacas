/** \file lispeval.h
 *  Evaluation of expressions.
 *
 */

#ifndef __lispeval_h__
#define __lispeval_h__

#include "yacasbase.h"
#include "lispobject.h"
#include "lispenvironment.h"

/*
void InternalEval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression);
*/

class UserStackInformation : public YacasBase
{
public:
    UserStackInformation():
        iRulePrecedence(-1),iSide(0)
    {
#ifdef YACAS_DEBUG
        iFileName = "(no file)";
        iLine = 0;
#endif
    }
    LispPtr iOperator;
    LispPtr iExpression;
    LispInt iRulePrecedence;
    LispInt iSide; // 0=pattern, 1=body

#ifdef YACAS_DEBUG
    LispCharPtr iFileName;
    LispInt iLine;
#endif
    
};



class LispEvaluatorBase : public YacasBase
{
public:
    virtual ~LispEvaluatorBase();
    virtual void Eval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression)=0;
    virtual void ResetStack();
    virtual UserStackInformation& StackInformation();
    virtual void ShowStack(LispEnvironment& aEnvironment, LispOutput& aOutput);
private:
    UserStackInformation iBasicInfo;
};


class BasicEvaluator : public LispEvaluatorBase
{
public:
    virtual void Eval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression);
};

class TracedEvaluator : public BasicEvaluator
{
public:
    virtual void Eval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression);
    virtual void Interact();
};


class TracedStackEvaluator : public BasicEvaluator
{
public:
    ~TracedStackEvaluator();
    virtual void Eval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression);
    virtual void ResetStack();
    virtual UserStackInformation& StackInformation();
    virtual void ShowStack(LispEnvironment& aEnvironment, LispOutput& aOutput);

private:
    void PushFrame();
    void PopFrame();
private:
    CArrayGrower<UserStackInformation*> objs;
};



/* GetUserFunction : get user function, possibly loading the required
   files to read in the function definition */
LispUserFunction* GetUserFunction(LispEnvironment& aEnvironment,
                                  LispPtr* subList);


/* Tracing functions */
void TraceShowEnter(LispEnvironment& aEnvironment,
                    LispPtr& aExpression);
void TraceShowLeave(LispEnvironment& aEnvironment, LispPtr& aResult,
                    LispPtr& aExpression);
void TraceShowArg(LispEnvironment& aEnvironment,LispPtr& aParam,
                  LispPtr& aValue);


void ShowExpression(LispString& outString, LispEnvironment& aEnvironment,
                    LispPtr& aExpression);


class YacasDebuggerBase : public YacasBase
{
public:
    virtual ~YacasDebuggerBase();
    virtual void Start() = 0;
    virtual void Finish() = 0;
    virtual void Enter(LispEnvironment& aEnvironment, 
                       LispPtr& aExpression) = 0;
    virtual void Leave(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aExpression) = 0;
};

class DefaultDebugger : public YacasDebuggerBase
{
public:
    virtual void Start();
    virtual void Finish();
    virtual void Enter(LispEnvironment& aEnvironment, 
                       LispPtr& aExpression);
    virtual void Leave(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aExpression);
};


#endif



