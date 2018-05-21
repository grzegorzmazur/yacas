
#include "yacas/lispeval.h"
#include "yacas/lispuserfunc.h"
#include "yacas/standard.h"

#include "yacas/errors.h"
#include "yacas/infixparser.h"
#include "yacas/lispio.h"
#include "yacas/platfileio.h"

#include <regex>
#include <sstream>

LispUserFunction* GetUserFunction(LispEnvironment& aEnvironment,
                                  LispPtr* subList)
{
    LispObject* head = (*subList);
    LispUserFunction* userFunc = aEnvironment.UserFunction(*subList);
    if (userFunc) {
        return userFunc;
    } else if (head->String() != nullptr) {
        LispMultiUserFunction* multiUserFunc =
            aEnvironment.MultiUserFunction(head->String());
        if (multiUserFunc->iFileToOpen != nullptr) {
            LispDefFile* def = multiUserFunc->iFileToOpen;
            multiUserFunc->iFileToOpen = nullptr;
            InternalUse(aEnvironment, def->FileName());
        }
        userFunc = aEnvironment.UserFunction(*subList);
    }
    return userFunc;
}

UserStackInformation& LispEvaluatorBase::StackInformation()
{
    return iBasicInfo;
}

void LispEvaluatorBase::ResetStack() {}

void LispEvaluatorBase::ShowStack(LispEnvironment& aEnvironment,
                                  std::ostream& aOutput)
{
}

// Eval: evaluates an expression. The result of this operation must
// be a unique (copied) element! Eg. its Nixed might be set...
void BasicEvaluator::Eval(LispEnvironment& aEnvironment,
                          LispPtr& aResult,
                          LispPtr& aExpression)
{
    assert(aExpression);

    if (aEnvironment.stop_evaluation) {
        aEnvironment.stop_evaluation = false;
        ShowStack(aEnvironment, aEnvironment.CurrentOutput());
        throw LispErrUserInterrupt();
    }

    aEnvironment.iEvalDepth++;
    if (aEnvironment.iEvalDepth >= aEnvironment.iMaxEvalDepth) {
        ShowStack(aEnvironment, aEnvironment.CurrentOutput());
        throw LispErrMaxRecurseDepthReached();
    }

    const LispString* str = aExpression->String();

    // Evaluate an atom: find the bound value (treat it as a variable)
    if (str) {
        if (str->front() == '\"') {
            aResult = aExpression->Copy();
            goto FINISH;
        }

        LispPtr val;
        aEnvironment.GetVariable(str, val);
        if (!!val) {
            aResult = (val->Copy());
            goto FINISH;
        }
        aResult = (aExpression->Copy());
        goto FINISH;
    }

    {
        LispPtr* subList = aExpression->SubList();

        if (subList) {
            LispObject* head = (*subList);
            if (head) {
                if (head->String()) {
                    {
                        const auto i =
                            aEnvironment.CoreCommands().find(head->String());
                        if (i != aEnvironment.CoreCommands().end()) {
                            i->second.Evaluate(aResult, aEnvironment, *subList);
                            goto FINISH;
                        }
                    }

                    {
                        LispUserFunction* userFunc;
                        userFunc = GetUserFunction(aEnvironment, subList);
                        if (userFunc) {
                            userFunc->Evaluate(aResult, aEnvironment, *subList);
                            goto FINISH;
                        }
                    }
                } else {
                    LispPtr oper((*subList));
                    LispPtr args2((*subList)->Nixed());
                    InternalApplyPure(oper, args2, aResult, aEnvironment);
                    goto FINISH;
                }
                ReturnUnEvaluated(aResult, *subList, aEnvironment);
                goto FINISH;
            }
        }
        aResult = (aExpression->Copy());
    }
FINISH:
    aEnvironment.iEvalDepth--;
}

void ShowExpression(LispString& outString,
                    LispEnvironment& aEnvironment,
                    LispPtr& aExpression)
{
    InfixPrinter infixprinter(aEnvironment.PreFix(),
                              aEnvironment.InFix(),
                              aEnvironment.PostFix(),
                              aEnvironment.Bodied());
    // Print out the current expression
    std::ostringstream stream;
    infixprinter.Print(aExpression, stream, aEnvironment);
    outString.append(stream.str());

    std::regex_replace(
        outString, std::regex("(^\")|([^\\\\]\")"), std::string("\\\""));
}

static void TraceShowExpression(LispEnvironment& aEnvironment,
                                LispPtr& aExpression)
{
    LispString outString;
    ShowExpression(outString, aEnvironment, aExpression);
    aEnvironment.CurrentOutput().write(outString.c_str(), outString.size());
}

void TraceShowArg(LispEnvironment& aEnvironment,
                  LispPtr& aParam,
                  LispPtr& aValue)
{
    for (int i = 0; i < aEnvironment.iEvalDepth + 2; i++)
        aEnvironment.CurrentOutput().write("  ", 2);
    aEnvironment.CurrentOutput() << "TrArg(\"";
    TraceShowExpression(aEnvironment, aParam);
    aEnvironment.CurrentOutput() << "\",\"";
    TraceShowExpression(aEnvironment, aValue);
    aEnvironment.CurrentOutput() << "\");\n";
}

void TraceShowEnter(LispEnvironment& aEnvironment, LispPtr& aExpression)
{
    for (int i = 0; i < aEnvironment.iEvalDepth; i++)
        aEnvironment.CurrentOutput().write("  ", 2);
    aEnvironment.CurrentOutput() << "TrEnter(\"";
    {
        const char* function = "";
        if (aExpression->SubList()) {
            LispPtr* sub = aExpression->SubList();
            if ((*sub)->String())
                function = (*sub)->String()->c_str();
        }
        aEnvironment.CurrentOutput() << function;
    }
    aEnvironment.CurrentOutput() << "\",\"";
    TraceShowExpression(aEnvironment, aExpression);
    aEnvironment.CurrentOutput() << "\",\"";
    aEnvironment.CurrentOutput() << ""; // file
    aEnvironment.CurrentOutput() << "\",";
    aEnvironment.CurrentOutput() << "0"; // line
    aEnvironment.CurrentOutput() << ");\n";
}

void TraceShowLeave(LispEnvironment& aEnvironment,
                    LispPtr& aResult,
                    LispPtr& aExpression)
{
    for (int i = 0; i < aEnvironment.iEvalDepth; i++)
        aEnvironment.CurrentOutput().write("  ", 2);
    aEnvironment.CurrentOutput().write("TrLeave(\"", 9);
    TraceShowExpression(aEnvironment, aExpression);
    aEnvironment.CurrentOutput().write("\",\"", 3);
    TraceShowExpression(aEnvironment, aResult);
    aEnvironment.CurrentOutput().write("\");\n", 4);
}

void TracedStackEvaluator::PushFrame()
{
    UserStackInformation* op = new UserStackInformation;
    objs.push_back(op);
}

void TracedStackEvaluator::PopFrame()
{
    assert(!objs.empty());
    delete objs.back();
    objs.pop_back();
}

void TracedStackEvaluator::ResetStack()
{
    while (!objs.empty())
        PopFrame();
}

UserStackInformation& TracedStackEvaluator::StackInformation()
{
    return *objs.back();
}

TracedStackEvaluator::~TracedStackEvaluator()
{
    ResetStack();
}

void TracedStackEvaluator::ShowStack(LispEnvironment& aEnvironment,
                                     std::ostream& aOutput)
{
    LispLocalEvaluator local(aEnvironment, new BasicEvaluator);

    const std::size_t upto = objs.size();

    for (std::size_t i = 0; i < upto; ++i) {
        aEnvironment.CurrentOutput() << i << ": ";
        aEnvironment.CurrentPrinter().Print(
            objs[i]->iOperator, aEnvironment.CurrentOutput(), aEnvironment);

        int internal;
        internal =
            aEnvironment.CoreCommands().find(objs[i]->iOperator->String()) !=
            aEnvironment.CoreCommands().end();
        if (internal) {
            aEnvironment.CurrentOutput() << " (Internal function) ";
        } else {
            if (objs[i]->iRulePrecedence >= 0) {
                aEnvironment.CurrentOutput()
                    << " (Rule # " << objs[i]->iRulePrecedence;
                if (objs[i]->iSide)
                    aEnvironment.CurrentOutput() << " in body) ";
                else
                    aEnvironment.CurrentOutput() << " in pattern) ";
            } else
                aEnvironment.CurrentOutput() << " (User function) ";
        }
        if (!!objs[i]->iExpression) {
            aEnvironment.CurrentOutput() << "\n      ";
            if (aEnvironment.iEvalDepth > (aEnvironment.iMaxEvalDepth - 10)) {
                LispString expr;
                PrintExpression(expr, objs[i]->iExpression, aEnvironment, 60);
                aEnvironment.CurrentOutput() << expr;
            } else {
                LispPtr* subList = objs[i]->iExpression->SubList();
                if (!!subList && !!(*subList)) {
                    LispString expr;
                    LispPtr out(objs[i]->iExpression);
                    PrintExpression(expr, out, aEnvironment, 60);
                    aEnvironment.CurrentOutput() << expr;
                }
            }
        }
        aEnvironment.CurrentOutput() << '\n';
    }
}

void TracedStackEvaluator::Eval(LispEnvironment& aEnvironment,
                                LispPtr& aResult,
                                LispPtr& aExpression)
{
    if (aEnvironment.iEvalDepth >= aEnvironment.iMaxEvalDepth) {
        ShowStack(aEnvironment, aEnvironment.CurrentOutput());
        throw LispErrMaxRecurseDepthReached();
    }

    LispPtr* subList = aExpression->SubList();
    const LispString* str = nullptr;
    if (subList) {
        LispObject* head = (*subList);
        if (head) {
            str = head->String();
            if (str) {
                PushFrame();
                UserStackInformation& st = StackInformation();
                st.iOperator = LispAtom::New(aEnvironment, *str);
                st.iExpression = aExpression;
            }
        }
    }
    BasicEvaluator::Eval(aEnvironment, aResult, aExpression);
    if (str) {
        PopFrame();
    }
}

void TracedEvaluator::Eval(LispEnvironment& aEnvironment,
                           LispPtr& aResult,
                           LispPtr& aExpression)
{
    if (!aEnvironment.iDebugger)
        throw LispErrGeneric("Internal error: debugging failing");
    if (aEnvironment.iDebugger->Stopped())
        throw LispErrGeneric("");

REENTER:
    errorOutput.clear();
    errorOutput.str("");

    try {
        aEnvironment.iDebugger->Enter(aEnvironment, aExpression);
    } catch (const LispError& error) {
        HandleError(error, aEnvironment, errorOutput);
    }

    if (aEnvironment.iDebugger->Stopped())
        throw LispErrGeneric("");

    if (!errorOutput.str().empty()) {
        aEnvironment.CurrentOutput() << errorOutput.str();
        aEnvironment.iEvalDepth = 0;
        goto REENTER;
    }

    errorOutput.clear();
    errorOutput.str("");

    try {
        BasicEvaluator::Eval(aEnvironment, aResult, aExpression);
    } catch (const LispError& error) {
        HandleError(error, aEnvironment, errorOutput);
    }

    if (!errorOutput.str().empty()) {
        aEnvironment.CurrentOutput() << errorOutput.str();
        aEnvironment.iEvalDepth = 0;
        aEnvironment.iDebugger->Error(aEnvironment);
        goto REENTER;
    }

    if (aEnvironment.iDebugger->Stopped())
        throw LispErrGeneric("");

    aEnvironment.iDebugger->Leave(aEnvironment, aResult, aExpression);
    if (aEnvironment.iDebugger->Stopped())
        throw LispErrGeneric("");
}

void DefaultDebugger::Start() {}

void DefaultDebugger::Finish() {}

void DefaultDebugger::Enter(LispEnvironment& aEnvironment, LispPtr& aExpression)
{
    LispLocalEvaluator local(aEnvironment, new BasicEvaluator);
    iTopExpr = (aExpression->Copy());
    LispPtr result;
    defaultEval.Eval(aEnvironment, result, iEnter);
}

void DefaultDebugger::Leave(LispEnvironment& aEnvironment,
                            LispPtr& aResult,
                            LispPtr& aExpression)
{
    LispLocalEvaluator local(aEnvironment, new BasicEvaluator);
    LispPtr result;
    iTopExpr = (aExpression->Copy());
    iTopResult = (aResult);
    defaultEval.Eval(aEnvironment, result, iLeave);
}

bool DefaultDebugger::Stopped()
{
    return iStopped;
}

void DefaultDebugger::Error(LispEnvironment& aEnvironment)
{
    LispLocalEvaluator local(aEnvironment, new BasicEvaluator);
    LispPtr result;
    defaultEval.Eval(aEnvironment, result, iError);
}
