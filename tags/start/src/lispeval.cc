 
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
    LispUserFunction* userFunc = aEnvironment.UserFunction(*subList);
    if (userFunc != NULL)
    {
        return userFunc;
    }
    else if (head->String()!=NULL)
    {
        LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(head->String());
        if (multiUserFunc->iFileToOpen!=NULL)
        {
            LispDefFile* def = multiUserFunc->iFileToOpen;
#ifdef YACAS_DEBUG
            /*Show loading... */
            printf("Debug> Loading file %s for function %s\n",def->iFileName->String(),head->String()->String());
#endif
            multiUserFunc->iFileToOpen=NULL;
            InternalUse(aEnvironment,def->iFileName);

#ifdef YACAS_DEBUG
            printf("Debug> Finished loading file %s\n",def->iFileName->String());
#endif
            userFunc = aEnvironment.UserFunction(*subList);
        }
    }
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
        EvalFuncBase* func = aExpression.Get()->EvalFunc();
        LispPtr* subList = aExpression.Get()->SubList();

        /*TODO I have to be REALLY sure about this one... */
        if (func)
        {
            func->Evaluate(aResult, aEnvironment, *subList);
            goto FINISH;
        }
        /* */
        if (subList)
        {
            LispObject* head = subList->Get();
            if (head)
            {
                LispEvaluator* evaluator =
                    aEnvironment.Commands().LookUp(head->String());

                // Try to find a built-in command
                if (evaluator)
                {
                aExpression.Get()->SetEvalFunc(evaluator);
                evaluator->Evaluate(aResult, aEnvironment, *subList);
                goto FINISH;
                }
                // Else try to find a user-defined function
                else
                {

                    LispUserFunction* userFunc;

                    userFunc = GetUserFunction(aEnvironment, subList);
                    if (userFunc != NULL)
                    {
                        aExpression.Get()->SetEvalFunc(userFunc);
                        userFunc->Evaluate(aResult,aEnvironment,*subList);
                        goto FINISH;
                    }
                }
            }
        }
        aResult.Set(aExpression.Get()->Copy(LispFalse));
    }
FINISH:

    aEnvironment.iEvalDepth--;
}

void TraceShowArg(LispEnvironment& aEnvironment,LispPtr& aParam,
                  LispPtr& aValue)
{
    LispInt i;
    InfixPrinter infixprinter(aEnvironment.PreFix(),
                              aEnvironment.InFix(),
                              aEnvironment.PostFix(),
                              aEnvironment.Bodied());
    for (i=0;i<aEnvironment.iEvalDepth+2;i++)
        aEnvironment.CurrentOutput()->Write("  ");
    aEnvironment.CurrentOutput()->Write("TrArg(");
    infixprinter.Print(aParam,*aEnvironment.CurrentOutput());
    aEnvironment.CurrentOutput()->Write(",");
    infixprinter.Print(aValue,*aEnvironment.CurrentOutput());
    aEnvironment.CurrentOutput()->Write(");\n");
}

void TraceShowEnter(LispEnvironment& aEnvironment,
                    LispPtr& aExpression)
{
    
    
    LispInt i;
    for (i=0;i<aEnvironment.iEvalDepth;i++)
        aEnvironment.CurrentOutput()->Write("  ");
    aEnvironment.CurrentOutput()->Write("TrEnter(");
    InfixPrinter infixprinter(aEnvironment.PreFix(),
                              aEnvironment.InFix(),
                              aEnvironment.PostFix(),
                              aEnvironment.Bodied());

    infixprinter.Print(aExpression, *aEnvironment.CurrentOutput());
    aEnvironment.CurrentOutput()->Write(");\n");
}



void TraceShowLeave(LispEnvironment& aEnvironment, LispPtr& aResult,
                    LispPtr& aExpression)
{
    
    LispInt i;
    for (i=0;i<aEnvironment.iEvalDepth;i++)
        aEnvironment.CurrentOutput()->Write("  ");
    aEnvironment.CurrentOutput()->Write("TrLeave(");
    InfixPrinter infixprinter(aEnvironment.PreFix(),
                              aEnvironment.InFix(),
                              aEnvironment.PostFix(),
                              aEnvironment.Bodied());

    infixprinter.Print(aExpression, *aEnvironment.CurrentOutput());
    aEnvironment.CurrentOutput()->Write(",");
    infixprinter.Print(aResult, *aEnvironment.CurrentOutput());
    aEnvironment.CurrentOutput()->Write(");\n");
}


void TracedStackEvaluator::PushFrame()
{
    UserStackInformation *op = new UserStackInformation;
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
    LispInt i;
    LispInt from=0;
    LispInt upto = objs.NrItems();
    if (upto-from > 16)
        from = upto-16;
    for (i=from;i<upto;i++)
    {
        LispChar str[20];
        InternalIntToAscii(str,i);
        aEnvironment.CurrentOutput()->Write("Debug> ");
        aEnvironment.CurrentOutput()->Write(str);
        aEnvironment.CurrentOutput()->Write(" : ");
        aEnvironment.CurrentPrinter().Print(objs[i]->iOperator, *aEnvironment.CurrentOutput());

        if (aEnvironment.Commands().LookUp(objs[i]->iOperator.Get()->String()))
        {
            aEnvironment.CurrentOutput()->Write(" (Internal function)");
        }
        else
        {
            if (objs[i]->iRulePrecedence>=0)
            {
                aEnvironment.CurrentOutput()->Write(" (Rule # ");
                InternalIntToAscii(str,objs[i]->iRulePrecedence);
                aEnvironment.CurrentOutput()->Write(str);
                if (objs[i]->iSide)
                    aEnvironment.CurrentOutput()->Write(" in body)");
                else
                    aEnvironment.CurrentOutput()->Write(" in pattern)");
            }
            else
                aEnvironment.CurrentOutput()->Write(" (User function)");
            }

        aEnvironment.CurrentOutput()->Write("\n");
    }
}


void TracedStackEvaluator::Eval(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aExpression)
{
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
                StackInformation().iOperator.Set(LispAtom::New(str));
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
    TraceShowEnter(aEnvironment, aExpression);

    Interact();
    BasicEvaluator::Eval(aEnvironment, aResult, aExpression);

    TraceShowLeave(aEnvironment, aResult, aExpression);
    Interact();
}

void TracedEvaluator::Interact()
{
//    getchar();
}



