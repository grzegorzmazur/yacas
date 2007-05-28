 
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
  LispObject* head = (*subList);
  LispUserFunction* userFunc = aEnvironment.UserFunction(*subList);
  if (userFunc)
  {
    return userFunc;
  }
  else if (head->String()!=NULL)
  {
    LispMultiUserFunction* multiUserFunc = aEnvironment.MultiUserFunction(head->String());
    if (multiUserFunc->iFileToOpen!=NULL)
    {
      LispDefFile* def = multiUserFunc->iFileToOpen;
#ifdef YACAS_DEBUG
      /*Show loading... */
      {
        extern int verbose_debug;
        if (verbose_debug)
        {
          char buf[1024];
        #ifdef HAVE_VSNPRINTF
          snprintf(buf,1024,"Debug> Loading file %s for function %s\n",def->iFileName->c_str(),head->String()->c_str());
        #else
          sprintf(buf,      "Debug> Loading file %s for function %s\n",def->iFileName->c_str(),head->String()->c_str());
        #endif
          aEnvironment.CurrentOutput()->Write(buf);
        }
      }
#endif
      multiUserFunc->iFileToOpen=NULL;
      InternalUse(aEnvironment,def->iFileName);

#ifdef YACAS_DEBUG
      {
        extern int verbose_debug;
        if (verbose_debug)
        {
          char buf[1024];
          #ifdef HAVE_VSNPRINTF
            snprintf(buf,1024,"Debug> Finished loading file %s\n",def->iFileName->c_str());
          #else
            sprintf(buf,      "Debug> Finished loading file %s\n",def->iFileName->c_str());
          #endif
          aEnvironment.CurrentOutput()->Write(buf);
        }
      }
#endif
    }
    userFunc = aEnvironment.UserFunction(*subList);
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

BasicEvaluator::~BasicEvaluator()
{
}


// Eval: evaluates an expression. The result of this operation must
// be a unique (copied) element! Eg. its Nixed might be set...
void BasicEvaluator::Eval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression)
{
  LISPASSERT(aExpression);

  aEnvironment.iEvalDepth++;
  if (aEnvironment.iEvalDepth>=aEnvironment.iMaxEvalDepth)
  {
    CHK2(aEnvironment.iEvalDepth<aEnvironment.iMaxEvalDepth,
    (aEnvironment.iEvalDepth>aEnvironment.iMaxEvalDepth+20)
    ? KLispErrUserInterrupt : KLispErrMaxRecurseDepthReached);
  }

  LispString * str = aExpression->String();

  // Evaluate an atom: find the bound value (treat it as a variable)
  if (str)
  {
    if (str->c_str()[0] == '\"')
    {
      aResult = (aExpression->Copy());
      goto FINISH;
    }

    LispPtr val;
    aEnvironment.GetVariable(str,val);
    if (!!val)
    {
      aResult = (val->Copy());
      goto FINISH;
    }
    aResult = (aExpression->Copy());
    goto FINISH;
  }

  {
    LispPtr* subList = aExpression->SubList();

    if (subList)
    {
      LispObject* head = (*subList);
      if (head)
      {
        if (head->String())
        {
          {
            YacasEvaluator* evaluator = aEnvironment.CoreCommands().LookUp(head->String());
            // Try to find a built-in command
            if (evaluator)
            {
              evaluator->Evaluate(aResult, aEnvironment, *subList);
              goto FINISH;
            }
          }

          {
            LispUserFunction* userFunc;
            userFunc = GetUserFunction(aEnvironment, subList);
            if (userFunc)
            {
              userFunc->Evaluate(aResult,aEnvironment,*subList);
              goto FINISH;
            }
          }
        }
        else
        {
          //printf("ApplyPure!\n");
          LispPtr oper((*subList));
          LispPtr args2((*subList)->Nixed());
          InternalApplyPure(oper,args2,aResult,aEnvironment);
          goto FINISH;
        }
        //printf("**** Undef: %s\n",head->String()->c_str());
        ReturnUnEvaluated(aResult,*subList,aEnvironment);
        goto FINISH;
      }
    }
    aResult = (aExpression->Copy());
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
  for (LispInt i = outString.Size()-1; i >= 0; --i)
  {
    if (outString[i] == '\"')
      outString.Insert(i, LispChar('\\'));
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
  for (LispInt i=0;i<aEnvironment.iEvalDepth+2;i++)
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
  for (LispInt i=0;i<aEnvironment.iEvalDepth;i++)
    aEnvironment.CurrentOutput()->Write("  ");
  aEnvironment.CurrentOutput()->Write("TrEnter(\"");
  {
    LispChar * function = "";
    if (aExpression->SubList())
    {
      LispPtr *sub = aExpression->SubList();
      if ((*sub)->String())
        function = (*sub)->String()->c_str();
    }
    aEnvironment.CurrentOutput()->Write(function);
  }
  aEnvironment.CurrentOutput()->Write("\",\"");
  TraceShowExpression(aEnvironment, aExpression);
  aEnvironment.CurrentOutput()->Write("\",\"");
#ifdef YACAS_DEBUG
  aEnvironment.CurrentOutput()->Write(
  aExpression->iFileName ? aExpression->iFileName : ""); //file
  aEnvironment.CurrentOutput()->Write("\",");
  LispChar buf[30];
  InternalIntToAscii(buf,aExpression->iLine);
  aEnvironment.CurrentOutput()->Write(buf); //line
#else
  aEnvironment.CurrentOutput()->Write(""); //file
  aEnvironment.CurrentOutput()->Write("\",");
  aEnvironment.CurrentOutput()->Write("0"); //line
#endif
  aEnvironment.CurrentOutput()->Write(");\n");
}

void TraceShowLeave(LispEnvironment& aEnvironment, LispPtr& aResult,
                    LispPtr& aExpression)
{
  for (LispInt i=0;i<aEnvironment.iEvalDepth;i++)
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
  LISPASSERT (objs.Size() > 0);
  if (objs[objs.Size()-1])
  {
    delete objs[objs.Size()-1];
    objs[objs.Size()-1] = NULL;
  }
  objs.Delete(objs.Size()-1);
}

void TracedStackEvaluator::ResetStack()
{
  while (objs.Size()>0)
  {
    PopFrame();
  }
}

UserStackInformation& TracedStackEvaluator::StackInformation()
{
  return *(objs[objs.Size()-1]);
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
  LispInt upto = objs.Size();
 
  for (i=from;i<upto;i++)
  {
    LispChar str[20];
#ifdef YACAS_DEBUG
    aEnvironment.CurrentOutput()->Write(objs[i]->iFileName);
    aEnvironment.CurrentOutput()->Write("(");
    InternalIntToAscii(str,objs[i]->iLine);
    aEnvironment.CurrentOutput()->Write(str);
    aEnvironment.CurrentOutput()->Write(") : ");
    aEnvironment.CurrentOutput()->Write("Debug> ");
#endif
    InternalIntToAscii(str,i);
    aEnvironment.CurrentOutput()->Write(str);
    aEnvironment.CurrentOutput()->Write(": ");
    aEnvironment.CurrentPrinter().Print(objs[i]->iOperator, *aEnvironment.CurrentOutput(),aEnvironment);

    LispInt internal;
    internal = (NULL != aEnvironment.CoreCommands().LookUp(objs[i]->iOperator->String()));
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
    if (!!objs[i]->iExpression)
    {
      aEnvironment.CurrentOutput()->Write("\n      ");
      if (aEnvironment.iEvalDepth>(aEnvironment.iMaxEvalDepth-10))
      {
        LispString expr;
        PrintExpression(expr, objs[i]->iExpression,aEnvironment,60);
        aEnvironment.CurrentOutput()->Write(expr.c_str());
      }
      else
      {
        LispPtr* subList = objs[i]->iExpression->SubList();
        if (!!subList && !!(*subList))
        {
          LispString expr;
          LispPtr out(objs[i]->iExpression);
          PrintExpression(expr, out,aEnvironment,60);
          aEnvironment.CurrentOutput()->Write(expr.c_str());
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

  LispPtr* subList = aExpression->SubList();
  LispString * str = NULL;
  if (subList)
  {
    LispObject* head = (*subList);
    if (head)
    {
      str = head->String();
      if (str)
      {
        PushFrame();
        UserStackInformation& st = StackInformation();
        st.iOperator = (LispAtom::New(aEnvironment,str->c_str()));
        st.iExpression = (aExpression);
#ifdef YACAS_DEBUG
        if (aExpression->iFileName)
        {
          st.iFileName = aExpression->iFileName;
          st.iLine = aExpression->iLine;
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
  if(!aEnvironment.iDebugger) RaiseError("Internal error: debugging failing");
  if(aEnvironment.iDebugger->Stopped()) RaiseError("");

REENTER:
  errorStr.ResizeTo(1); errorStr[0] = '\0';
  LispTrap(aEnvironment.iDebugger->Enter(aEnvironment, aExpression),errorOutput,aEnvironment);
  if(aEnvironment.iDebugger->Stopped()) RaiseError("");
  if (errorStr[0])
  {
    aEnvironment.CurrentOutput()->Write(errorStr.c_str());
    aEnvironment.iEvalDepth=0;
    goto REENTER;
  }

  errorStr.ResizeTo(1); errorStr[0] = '\0';
  LispTrap(BasicEvaluator::Eval(aEnvironment, aResult, aExpression),errorOutput,aEnvironment);

  if (errorStr[0])
  {
    aEnvironment.CurrentOutput()->Write(errorStr.c_str());
    aEnvironment.iEvalDepth=0;
    aEnvironment.iDebugger->Error(aEnvironment);
    goto REENTER;
  }

  if(aEnvironment.iDebugger->Stopped()) RaiseError("");

  aEnvironment.iDebugger->Leave(aEnvironment, aResult, aExpression);
  if(aEnvironment.iDebugger->Stopped()) RaiseError("");
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
  LispLocalEvaluator local(aEnvironment,NEW BasicEvaluator);
  iTopExpr = (aExpression->Copy());
  LispPtr result;
  defaultEval.Eval(aEnvironment, result, iEnter);
}

void DefaultDebugger::Leave(LispEnvironment& aEnvironment, LispPtr& aResult,
                                    LispPtr& aExpression)
{
  LispLocalEvaluator local(aEnvironment,NEW BasicEvaluator);
  LispPtr result;
  iTopExpr = (aExpression->Copy());
  iTopResult = (aResult);
  defaultEval.Eval(aEnvironment, result, iLeave);
}

LispBoolean DefaultDebugger::Stopped()
{
  return iStopped;
}

void DefaultDebugger::Error(LispEnvironment& aEnvironment)
{
  LispLocalEvaluator local(aEnvironment,NEW BasicEvaluator);
  LispPtr result;
  defaultEval.Eval(aEnvironment, result, iError);
}
