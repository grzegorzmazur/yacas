

#include <stdio.h>
#include <stdlib.h>
#include "debugclass.h"

ProteusDebugger::ProteusDebugger(LispCharPtr aTempDir)
{
    iTempDir = aTempDir;
    traceStack = NULL;
    LispInt traceFile = 0;
    LispInt running = 0;
}

ProteusDebugger::~ProteusDebugger()
{
    Finish();
}

void ProteusDebugger::Dump()
{
    char filename[100];
    sprintf(filename,"%sexp.%d",iTempDir,traceFile);
    FILE* f = fopen(filename,"w");
    if (!f) return;

    LispInt i,nr = StackTop()->NrItems();
    LispInt spaces = 1+4*traceStack->NrItems();
    char* indent = (char*)malloc(spaces+1);
    memset(indent,' ',spaces);
    indent[spaces] = '\0';
    
    for (i=0;i<nr;i++)
    {
        DebugLine* debugLine = (*StackTop())[i];
    
//printf("%d %d\n",debugLine->iToEnter,debugLine->iLine);
        fprintf(f,"%d %d\n",debugLine->iToEnter,debugLine->iLine);
        if (debugLine->iLine > 0 && debugLine->iFile != NULL)
        {
//printf("%s\n",debugLine->iFile);
            fprintf(f,"%s\n",debugLine->iFile);
        }
//printf(indent);
        fprintf(f,indent);
        switch(debugLine->iType)
        {
        case KTypeEnter:
//printf("ENTER: ");
            fprintf(f,"ENTER: ");
            break;
        case KTypeLeave:
//printf("LEAVE: ");
            fprintf(f,"LEAVE: ");
            break;
        }
//printf("%s\n",debugLine->iExpr.String());
        fprintf(f,"%s\n",debugLine->iExpr.String());
    }
//printf("end\n");
    fprintf(f,"end\n");
   fclose(f);
}

void ProteusDebugger::AddStackFrame()
{
    traceStack->Append(new DebugStackFrame);
}
void ProteusDebugger::DropStackFrame()
{
    delete (*traceStack)[traceStack->NrItems()-1];
    traceStack->SetNrItems(traceStack->NrItems()-1);
}

void ProteusDebugger::Start()
{
    traceStack = new DebugStack;
    traceFile = 1;
    running = 1;
    AddStackFrame();
}
void ProteusDebugger::Finish()
{
    if (running)
    {
        running = 0;

        LispString result("STOPPED");
        while (traceStack->NrItems() > 1)
        {
            if (StackTop()->NrItems() > 0)
            {
                Dump();
                DropStackFrame();
                (*StackTop())[StackTop()->NrItems()-1]->iToEnter = traceFile;
                traceFile++;
            }
            else
            {
                DropStackFrame();
            }
            StackTop()->Append(new DebugLine(-1, KTypeLeave, result, NULL, 0));
        }
//        Dump();

        LispString begin("BEGIN");
        LispString end("END");
//        StackTop()->Append(new DebugLine(traceFile, KTypeEnter, begin, NULL, 0));
//        StackTop()->Append(new DebugLine(-1, KTypeLeave, end, NULL, 0));
        traceFile = 0;
        Dump();
        //AAA  traceStack := {{{traceFile,"BEGIN",""},{-1,"END",""}}};
//AAA  traceFile = 0;
//AAA  Dump();

        
        delete traceStack;
        traceStack = NULL;
    }
}

DebugStackFrame* ProteusDebugger::StackTop()
{
    return (*traceStack)[traceStack->NrItems()-1];
}
void ProteusDebugger::Enter(LispEnvironment& aEnvironment, 
                            LispPtr& aExpression)
{
    LispString expr;
    ShowExpression(expr, aEnvironment, aExpression);
    
    StackTop()->Append(new DebugLine(-1, KTypeEnter, expr, aExpression.Get()->iFileName, aExpression.Get()->iLine));
    AddStackFrame();
}
void ProteusDebugger::Leave(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispPtr& aExpression)
{
    if (StackTop()->NrItems() > 0)
    {
        Dump();
        DropStackFrame();
        (*StackTop())[StackTop()->NrItems()-1]->iToEnter = traceFile;
        traceFile++;
    }
    else
    {
        DropStackFrame();
    }

    LispString result;
    ShowExpression(result, aEnvironment, aResult);
    StackTop()->Append(new DebugLine(-1, KTypeLeave, result, NULL, 0));
}


