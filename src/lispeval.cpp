 
#include "yacasprivate.h"
#include "lispeval.h"
#include "lispuserfunc.h"
#include "standard.h"

#include "lispio.h"
#include "platfileio.h"
#include "infixparser.h"
#include "errors.h"




LispUserFunction* GetUserFunction(LispEnvironment& aEnvironment,
                                  LispPtr* subList)
{
    LispObject* head = subList->Get();
    LispUserFunction* userFunc = NULL;
    /**/
     userFunc = aEnvironment.UserFunction(*subList);
    CHECKPTR(userFunc);
    if (userFunc != NULL)
    {
    /*this doesn't work yet...
        LispMultiUserFunction* multiUserFunc =
            aEnvironment.MultiUserFunction(head->String());
        if (multiUserFunc->iFileToOpen!=NULL)
            if (!multiUserFunc->iFileToOpen->IsLoaded())
            {
                printf("Function %s\n",head->String()->String());
                LISPASSERT(multiUserFunc->iFileToOpen==NULL);
            }
            */
        return userFunc;
    }
    else
    /**/
    if (head->String()!=NULL)
    {
        LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(head->String());
//        CHECKPTR(multiUserFunc);
        if (multiUserFunc->iFileToOpen!=NULL)
        {
            LispDefFile* def = multiUserFunc->iFileToOpen;
//            CHECKPTR(def);
#ifdef YACAS_DEBUG
            /*Show loading... */
            printf("Debug> Loading file %s for function %s\n",def->iFileName()->String(),head->String()->String());
#endif
            multiUserFunc->iFileToOpen=NULL;
            InternalUse(aEnvironment,def->iFileName());

#ifdef YACAS_DEBUG
            printf("Debug> Finished loading file %s\n",def->iFileName()->String());
#endif
        }
        userFunc = aEnvironment.UserFunction(*subList);
    }
    CHECKPTR(userFunc);
    return userFunc;
}


UserStackInformation& LispEvaluatorBase::StackInformation()
{
    return iBasicInfo;
}
void LispEvaluatorBase::ResetStack()
{
}
void LispEvaluatorBase::ShowStack(LispEnvironment& aEnvironment, LispOutput& aOutput)
{
}
LispEvaluatorBase::~LispEvaluatorBase()
{
}


// Eval: evaluates an expression. The result of this operation must
// be a unique (copied) element! Eg. its Next might be set...
void BasicEvaluator::Eval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression)
{
    LISPASSERT(aExpression.Get() != NULL);

    aEnvironment.iEvalDepth++;
    if (aEnvironment.iEvalDepth>=aEnvironment.iMaxEvalDepth)
    {
        if (aEnvironment.iEvalDepth>aEnvironment.iMaxEvalDepth+20)
        {
            CHK2(aEnvironment.iEvalDepth<aEnvironment.iMaxEvalDepth,
                 KLispErrUserInterrupt);
        }
        else
        {
            CHK2(aEnvironment.iEvalDepth<aEnvironment.iMaxEvalDepth,
                 KLispErrMaxRecurseDepthReached);
        }
    }

    LispStringPtr str = aExpression.Get()->String();
    CHECKPTR(str);

    // Evaluate an atom: find the bound value (treat it as a variable)
    if (str)
    {
        if (str->String()[0] == '\"')
        {
            aResult.Set(aExpression.Get()->Copy(LispFalse));
            goto FINISH;
        }

        LispPtr val;
        aEnvironment.GetVariable(str,val);
        if (val.Get())
        {
            aResult.Set(val.Get()->Copy(LispFalse));
            goto FINISH;
        }
        aResult.Set(aExpression.Get()->Copy(LispFalse));
        goto FINISH;
    }

    {
//        EvalFuncBase* func = NULL;
        LispPtr* subList = aExpression.Get()->SubList();

//        if (func)
//        {
//            func->Evaluate(aResult, aEnvironment, *subList);
//            goto FINISH;
//        }
        if (subList)
        {
            LispObject* head = subList->Get();
            if (head)
            {
                if (head->String())
                {
                    LispEvaluator* evaluator =
                        aEnvironment.Commands().LookUp(head->String());
                    // Try to find a built-in command
                    if (evaluator)
                    {
                        evaluator->Evaluate(aResult, aEnvironment, *subList);
                        goto FINISH;
                    }

                    else // Else try to find a user-defined function
                    {
                        LispUserFunction* userFunc;
                        userFunc = GetUserFunction(aEnvironment, subList);
                        CHECKPTR(userFunc);
                        if (userFunc != NULL)
                        {
                            userFunc->Evaluate(aResult,aEnvironment,*subList);
                            goto FINISH;
                        }
                    }
                }
                else
                {
                    //printf("ApplyPure!\n");
                    LispPtr oper;
                    LispPtr args2;
                    oper.Set(subList->Get());
                    args2.Set(subList->Get()->Next().Get());
                    InternalApplyPure(oper,args2,aResult,aEnvironment);
                    goto FINISH;
                }
                //printf("**** Undef: %s\n",head->String()->String());
                ReturnUnEvaluated(aResult,*subList,aEnvironment);
                goto FINISH;
            }
        }
        aResult.Set(aExpression.Get()->Copy(LispFalse));
    }
FINISH:

    aEnvironment.iEvalDepth--;
}

void ShowExpression(LispString& outString, LispEnvironment& aEnvironment,
                    LispPtr& aExpression)
{
    InfixPrinter infixprinter(aEnvironment.PreFix(),
                              aEnvironment.InFix(),
                              aEnvironment.PostFix(),
                              aEnvironment.Bodied());
    // Print out the current expression
    StringOutput stream(outString);
    infixprinter.Print(aExpression, stream,aEnvironment);

    // Escape quotes
    LispInt i;
    LispChar c = '\\';
    for (i=outString.NrItems()-1;i>=0;--i)
    {
        switch(outString[i])
        {
        case '\"':
            outString.Insert(i,c);
        }
    }
}
static void TraceShowExpression(LispEnvironment& aEnvironment,
                                LispPtr& aExpression)
{
    LispString outString;
    ShowExpression(outString, aEnvironment, aExpression);
    aEnvironment.CurrentOutput()->Write(&outString[0]);
}


void TraceShowArg(LispEnvironment& aEnvironment,LispPtr& aParam,
                  LispPtr& aValue)
{
    LispInt i;
/*TODO remove?
    InfixPrinter infixprinter(aEnvironment.PreFix(),
                              aEnvironment.InFix(),
                              aEnvironment.PostFix(),
                              aEnvironment.Bodied());
                              */
    for (i=0;i<aEnvironment.iEvalDepth+2;i++)
        aEnvironment.CurrentOutput()->Write("  ");
    aEnvironment.CurrentOutput()->Write("TrArg(\"");
    TraceShowExpression(aEnvironment, aParam);
    aEnvironment.CurrentOutput()->Write("\",\"");
    TraceShowExpression(aEnvironment, aValue);
    aEnvironment.CurrentOutput()->Write("\");\n");
}


void TraceShowEnter(LispEnvironment& aEnvironment,
                    LispPtr& aExpression)
{
    LispInt i;
    for (i=0;i<aEnvironment.iEvalDepth;i++)
        aEnvironment.CurrentOutput()->Write("  ");
    aEnvironment.CurrentOutput()->Write("TrEnter(\"");
    {
        LispCharPtr function = "";
        if (aExpression.Get()->SubList() != NULL)
        {
            LispPtr *sub = aExpression.Get()->SubList();
            if (sub->Get()->String() != NULL)
                function = sub->Get()->String()->String();
        }
        aEnvironment.CurrentOutput()->Write(function);
    }
    aEnvironment.CurrentOutput()->Write("\",\"");
    TraceShowExpression(aEnvironment, aExpression);
    aEnvironment.CurrentOutput()->Write("\",\"");

#ifdef DEBUG_MODE
    if (aExpression.Get()->iFileName)
        aEnvironment.CurrentOutput()->Write(aExpression.Get()->iFileName); //file
    else
        aEnvironment.CurrentOutput()->Write(""); //file
#else
    aEnvironment.CurrentOutput()->Write(""); //file
#endif
    aEnvironment.CurrentOutput()->Write("\",");
#ifdef DEBUG_MODE
    {
        LispChar buf[30];
        InternalIntToAscii(buf,aExpression.Get()->iLine);
        aEnvironment.CurrentOutput()->Write(buf); //line
    }
#else
    aEnvironment.CurrentOutput()->Write("0"); //line
#endif

    aEnvironment.CurrentOutput()->Write(");\n");
}



void TraceShowLeave(LispEnvironment& aEnvironment, LispPtr& aResult,
                    LispPtr& aExpression)
{
    
    LispInt i;
    for (i=0;i<aEnvironment.iEvalDepth;i++)
        aEnvironment.CurrentOutput()->Write("  ");
    aEnvironment.CurrentOutput()->Write("TrLeave(\"");

    TraceShowExpression(aEnvironment, aExpression);
    aEnvironment.CurrentOutput()->Write("\",\"");
    TraceShowExpression(aEnvironment, aResult);
    aEnvironment.CurrentOutput()->Write("\");\n");
}


void TracedStackEvaluator::PushFrame()
{
    UserStackInformation *op = NEW UserStackInformation;
    objs.Append(op);
}
void TracedStackEvaluator::PopFrame()
{
    LISPASSERT (objs.NrItems() > 0);

    if (objs[objs.NrItems()-1] != NULL)
    {
        delete objs[objs.NrItems()-1];
        objs[objs.NrItems()-1] = NULL;
    }
    objs.Delete(objs.NrItems()-1);
}

void TracedStackEvaluator::ResetStack()
{
    while (objs.NrItems()>0)
    {
        PopFrame();
    }
}
UserStackInformation& TracedStackEvaluator::StackInformation()
{
    return *(objs[objs.NrItems()-1]);
}

TracedStackEvaluator::~TracedStackEvaluator()
{
    ResetStack();
}

void TracedStackEvaluator::ShowStack(LispEnvironment& aEnvironment, LispOutput& aOutput)
{
    LispLocalEvaluator local(aEnvironment,NEW BasicEvaluator);

    LispInt i;
    LispInt from=0;
    LispInt upto = objs.NrItems();
    
//#ifndef DEBUG_MODE
//    if (upto-from > 16)
//        from = upto-16;
//#endif
    for (i=from;i<upto;i++)
    {
        LispChar str[20];
#ifdef DEBUG_MODE
        {
            aEnvironment.CurrentOutput()->Write(objs[i]->iFileName);
            aEnvironment.CurrentOutput()->Write("(");
            InternalIntToAscii(str,objs[i]->iLine);
            aEnvironment.CurrentOutput()->Write(str);
            aEnvironment.CurrentOutput()->Write(") : ");
        }
#endif
        InternalIntToAscii(str,i);
#ifndef DEBUG_MODE
        aEnvironment.CurrentOutput()->Write("Debug> ");
#endif
        aEnvironment.CurrentOutput()->Write(str);
        aEnvironment.CurrentOutput()->Write(": ");
        aEnvironment.CurrentPrinter().Print(objs[i]->iOperator, *aEnvironment.CurrentOutput(),aEnvironment);

        LispInt internal;
        internal = (NULL != aEnvironment.Commands().LookUp(objs[i]->iOperator.Get()->String()));
        if (internal)
        {
            aEnvironment.CurrentOutput()->Write(" (Internal function) ");
        }
        else
        {
            if (objs[i]->iRulePrecedence>=0)
            {
                aEnvironment.CurrentOutput()->Write(" (Rule # ");
                InternalIntToAscii(str,objs[i]->iRulePrecedence);
                aEnvironment.CurrentOutput()->Write(str);
                if (objs[i]->iSide)
                    aEnvironment.CurrentOutput()->Write(" in body) ");
                else
                    aEnvironment.CurrentOutput()->Write(" in pattern) ");
            }
            else
                aEnvironment.CurrentOutput()->Write(" (User function) ");
        }
        if (objs[i]->iExpression.Get())
        {
            aEnvironment.CurrentOutput()->Write("\n      ");
            if (aEnvironment.iEvalDepth>(aEnvironment.iMaxEvalDepth-10))
            {
                LispString expr;
                PrintExpression(expr, objs[i]->iExpression,aEnvironment,60);
                aEnvironment.CurrentOutput()->Write(expr.String());
            }
            else
            {
                LispPtr* subList = objs[i]->iExpression.Get()->SubList();
                if (subList && subList->Get())
                {
                    LispString expr;
                    LispPtr out;
                    LispPtr in;
                    in.Set(subList->Get());
                    ReturnUnEvaluated(out,in,aEnvironment);
                    PrintExpression(expr, out,aEnvironment,60);
                    aEnvironment.CurrentOutput()->Write(expr.String());
                }
            }
        }

        aEnvironment.CurrentOutput()->Write("\n");
    }
}


void TracedStackEvaluator::Eval(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aExpression)
{

    if (aEnvironment.iEvalDepth>=aEnvironment.iMaxEvalDepth)
    {
        ShowStack(aEnvironment, *aEnvironment.CurrentOutput());
        CHK2(aEnvironment.iEvalDepth<aEnvironment.iMaxEvalDepth,
             KLispErrMaxRecurseDepthReached);
    }

    

#ifdef VLADIMIR
    InfixPrinter infixprinter(aEnvironment.PreFix(),
                              aEnvironment.InFix(),
                              aEnvironment.PostFix(),
                              aEnvironment.Bodied());

    LispString outString;
    StringOutput outStream(outString);
    infixprinter.Print(aExpression, outStream);
    LispCharPtr expression = outString.String();
//    printf("%s\n",expression);

#endif

    LispPtr* subList = aExpression.Get()->SubList();
    LispStringPtr str=NULL;
    if (subList)
    {
        LispObject* head;
        head = subList->Get();
        if (head)
        {
            str = head->String();
            if (str)
            {
                PushFrame();
                UserStackInformation& st = StackInformation();
                st.iOperator.Set(LispAtom::New(str));
                st.iExpression.Set(aExpression.Get());
#ifdef DEBUG_MODE
                if (aExpression.Get()->iFileName != NULL)
                {
                    st.iFileName = aExpression.Get()->iFileName;
                    st.iLine = aExpression.Get()->iLine;
                }
#endif
            }
        }
    }
    BasicEvaluator::Eval(aEnvironment, aResult, aExpression);
    if (str)
    {
        PopFrame();
    }
}


void TracedEvaluator::Eval(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aExpression)
{
    aEnvironment.iDebugger->Enter(aEnvironment, aExpression);
//TODO remove    TraceShowEnter(aEnvironment, aExpression);
//    Interact();
    BasicEvaluator::Eval(aEnvironment, aResult, aExpression);

    aEnvironment.iDebugger->Leave(aEnvironment, aResult, aExpression);
//    TraceShowLeave(aEnvironment, aResult, aExpression);
//    Interact();
}

void TracedEvaluator::Interact()
{
//    getchar();
}


YacasDebuggerBase::~YacasDebuggerBase()
{
}


void DefaultDebugger::Start()
{
}
void DefaultDebugger::Finish()
{
}
void DefaultDebugger::Enter(LispEnvironment& aEnvironment, 
                                    LispPtr& aExpression)
{
    TraceShowEnter(aEnvironment, aExpression);
}
void DefaultDebugger::Leave(LispEnvironment& aEnvironment, LispPtr& aResult,
                                    LispPtr& aExpression)
{
    TraceShowLeave(aEnvironment, aResult, aExpression);
}

