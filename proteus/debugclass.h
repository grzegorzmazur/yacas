
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
    inline DebugLine(LispInt aToEnter, LispInt aType, LispString& aExpr, LispCharPtr aFile, LispInt aLine)
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
    LispCharPtr iFile;
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
    ProteusDebugger(LispCharPtr aTempDir);
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
    LispCharPtr iTempDir;
};
#endif

