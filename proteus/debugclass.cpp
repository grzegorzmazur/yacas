

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "yacasprivate.h"
#include "debugclass.h"

FILE* fprofile;
char fprofilename[100];


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

int GetF(char* func,char* line)
{
    int i=0,nr=strlen(line);
    while (i<nr && isalpha(line[i])) i++;
    if (line[i] == '(')
    {
        memcpy(func,line,i);
        func[i] = '\0';
        return 1;
    }
    return 0;
}

void GetFuncName(char* line)
{
    char func[200];
    if (line[0] == '[')
    {
        strcpy(func,"Body");
    }
    else if (line[0] == '{')
    {
        strcpy(func,"List");
    }
    else if (GetF(func,line))
    {
    }
    else
    {
        return;
    }
    fprintf(fprofile,"%s\n",func);
}


void ProteusDebugger::Dump()
{
    char filename[100];
    sprintf(filename,"%sexp.%d",iTempDir,traceFile);
    FILE* f = fopen(filename,"w");
    if (!f) return;

    LispInt i,nr = StackTop()->NrItems();
    LispInt spaces = 1+4*traceStack->NrItems();
//    char* indent = (char*)malloc(spaces+1);
//    memset(indent,' ',spaces);
//    indent[spaces] = '\0';
    
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
        //        fprintf(f,indent);

        GetFuncName(debugLine->iExpr.String());
        
        fprintf(f,"%d ",spaces);
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
    traceStack->Append(NEW DebugStackFrame);
}
void ProteusDebugger::DropStackFrame()
{
    delete (*traceStack)[traceStack->NrItems()-1];
    traceStack->SetNrItems(traceStack->NrItems()-1);
}

void ProteusDebugger::Start()
{
    traceStack = NEW DebugStack;
    traceFile = 1;
    running = 1;
    AddStackFrame();

    sprintf(fprofilename,"%sprofile",iTempDir);
    fprofile = fopen(fprofilename,"w");
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

        fclose(fprofile);
        char cmd[500];
        sprintf(cmd,"sort %s > %s.sorted",fprofilename,fprofilename);
        system(cmd);
        {
            char func[200];
            int count=0;
            func[0]='\0';
            sprintf(cmd,"%s.sorted",fprofilename);
            FILE *f = fopen(cmd,"r");
            sprintf(cmd,"%s.counted",fprofilename);
            FILE *fout = fopen(cmd,"w");
            do
            {
                char nf[200];
                fscanf(f,"%s",nf);
                if (strcmp(nf,func) || feof(f))
                {
                    if (count && strlen(func))
                    {
                        fprintf(fout,"%d \t%s\n",count,func);
                    }
                    strcpy(func,nf);
                    count=1;
                }
                else
                {
                    count++;
                }
                
            } while (!feof(f));
            fclose(fout);
            fclose(f);
        }
        sprintf(cmd,"sort -n -r %s.counted > %s.sortcounted",fprofilename,fprofilename);
        system(cmd);
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


