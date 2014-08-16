
#include "yacas/yacasprivate.h"
#include "yacas/yacasbase.h"
#include "yacas/yacas.h"
#include "yacas/mathcommands.h"
#include "yacas/standard.h"

#ifdef YACAS_DEBUG
long theNrDefinedBuiltIn=0;
long theNrDefinedUser=0;
#endif


#define InternalEval env.iEvaluator->Eval

#define OPERATOR(kind,prec,name) \
  kind##operators.SetOperator(prec,hash.LookUp(#name));
// for example: OPERATOR(bodied,KMaxPrecedence,While) produces:
//    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("While"));


DefaultYacasEnvironment::~DefaultYacasEnvironment()
{
  delete output;
}



DefaultYacasEnvironment::DefaultYacasEnvironment(LispOutput* aOutput, LispInt aStackSize)
  : output(aOutput),hash(),printer(),coreCommands(),globals(),
    prefixoperators(),infixoperators(),postfixoperators(),bodiedoperators(),
    infixprinter(prefixoperators,
                 infixoperators,
                 postfixoperators,
                 bodiedoperators),
    userFunctions(),
    iEnvironment(coreCommands,userFunctions,
                 globals,hash,output,infixprinter,
                 prefixoperators,infixoperators,
                 postfixoperators,bodiedoperators,&input,aStackSize),
     input(iEnvironment.iInputStatus)
{
    // Define the built-in functions by tying their string representation
    // to a kernel callable routine.
#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) iEnvironment.SetCommand(fname,iname,nrargs,flags);
#define CORE_KERNEL_FUNCTION_ALIAS(iname,fname,nrargs,flags) iEnvironment.SetCommand(fname,iname,nrargs,flags);
#include "yacas/corefunctions.h"
#undef CORE_KERNEL_FUNCTION
#undef CORE_KERNEL_FUNCTION_ALIAS
#undef OPERATOR
}


CYacas::CYacas(LispOutput* aOutput, LispInt aStackSize):
    environment(aOutput ? aOutput : NEW StdUserOutput(), aStackSize),
    iResultOutput(iResult)
{
}


LISPEXPORT CYacas::~CYacas()
{
}




void CYacas::Evaluate(const LispChar * aExpression)
{
  LispEnvironment& env = environment.getEnv();
  LispInt stackTop = env.iStack.GetStackTop();

  iResult = "";
  env.iError = "";

    LispPtr result;

    try
     {
         LispPtr lispexpr;
//printf("Input: [%s]\n",aExpression);
         if (env.PrettyReader())
         {
            LispString * prettyReader = env.PrettyReader();
            LispString full(const_cast<LispChar *>(aExpression));  // TODO: woof
            full[full.size()-1] = ';';
            full.push_back('\0');
            StringInput input(full,env.iInputStatus);
            LispLocalInput localInput(env, &input);
            LispPtr args(nullptr);
            InternalApplyString(env, lispexpr,
                               prettyReader,
                               args);
         }
         else
         {
           LispString full((LispChar *)aExpression);
           full[full.size()-1] = ';';
           full.push_back('\0');
           StringInput input(full,env.iInputStatus);
           env.iInputStatus.SetTo("CommandLine");
           LispTokenizer &tok = *env.iCurrentTokenizer;
           InfixParser parser(tok, input,
                              env,
                              env.PreFix(),
                              env.InFix(),
                              env.PostFix(),
                              env.Bodied());
           parser.Parse(lispexpr);
         }

         env.iEvalDepth=0;
         env.iEvaluator->ResetStack();
         InternalEval(env, result, lispexpr);

         // If no error encountered, print result
         if (env.PrettyPrinter())
         {
             LispPtr nonresult;
             InternalApplyString(env, nonresult,
                                 env.PrettyPrinter(),
                                 result);
         }
         else
         {
             InfixPrinter infixprinter(env.PreFix(),
                                       env.InFix(),
                                       env.PostFix(),
                                       env.Bodied());

             infixprinter.Print(result, iResultOutput, env);
             iResultOutput.Write(";");
         }
         LispString * percent = env.HashTable().LookUp("%");
         env.SetVariable(percent,result,true);
     } catch (const LispError& error) {
        HandleError(error, env, env.iErrorOutput);
     }

     env.iStack.PopTo(stackTop);
}

const LispChar* CYacas::Result()
{
  return iResult.c_str();
}

const LispChar* CYacas::Error()
{
  LispEnvironment& env = environment.getEnv();
  return env.iError.c_str();
}



