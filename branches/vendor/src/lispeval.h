/** \file lispeval.h
 *  Evaluation of expressions.
 *
 */

#ifndef __lispeval_h__
#define __lispeval_h__

#include "lispobject.h"
#include "lispenvironment.h"

/*
void InternalEval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression);
*/

class UserStackInformation
{
public:
    UserStackInformation():
        iRulePrecedence(-1),iSide(0)
    {
    }
    LispPtr iOperator;
    LispInt iRulePrecedence;
    LispInt iSide; // 0=pattern, 1=body
};



class LispEvaluatorBase
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


#endif



