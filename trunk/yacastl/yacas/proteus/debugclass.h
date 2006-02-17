
#ifndef _debugclass_h_
#define _debugclass_h_

#include "lispeval.h"

enum
{
    KTypeEnter,
    KTypeLeave
};
class DebugLine
{
public:
    inline DebugLine(LispInt aToEnter, LispInt aType, LispString& aExpr, LispChar * aFile, LispInt aLine)
    {
        iToEnter = aToEnter;
        iType    = aType;
        iExpr    = aExpr.String();
        iFile    = aFile;
        iLine    = aLine;
    }
    LispInt     iToEnter;
    LispInt     iType;
    LispString  iExpr;
    LispChar * iFile;
    LispInt     iLine;
};


class DebugStackFrame : public CDeletingArrayGrower<DebugLine*>
{
};

class DebugStack : public CDeletingArrayGrower<DebugStackFrame*>
{
};

class ProteusDebugger : public YacasDebuggerBase
{
public:
    ProteusDebugger(LispChar * aTempDir);
    virtual ~ProteusDebugger();
    virtual void Start();
    virtual void Finish();
    virtual void Enter(LispEnvironment& aEnvironment, 
                       LispPtr& aExpression);
    virtual void Leave(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aExpression);

    void AddStackFrame();
    void DropStackFrame();
    void Dump();

private:
    DebugStackFrame* StackTop();

private:
    DebugStack *traceStack;
    LispInt traceFile;
    LispInt running;
    LispChar * iTempDir;
};
#endif

