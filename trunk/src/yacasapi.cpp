
#include "yacasprivate.h"
#include "yacasbase.h"
#include "yacas.h"
#include "mathcommands.h"
#include "standard.h"

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
#include "corefunctions.h"
#undef CORE_KERNEL_FUNCTION
#undef CORE_KERNEL_FUNCTION_ALIAS
#undef OPERATOR
}


LISPEXPORT CYacas* CYacas::NewL(LispOutput* aOutput,LispInt aStackSize)
{
  CYacas* self = NEW CYacas(
    aOutput ? aOutput : NEW StdUserOutput(), aStackSize);
  return self;
}

LISPEXPORT CYacas::~CYacas()
{
}


CYacas::CYacas(LispOutput* aOutput,LispInt aStackSize)
: environment(aOutput,aStackSize),iResult(),iResultOutput(iResult)
{
}



void CYacas::Evaluate(const LispChar * aExpression)
{
  LispEnvironment& env = environment.getEnv();
  LispInt stackTop = env.iStack.GetStackTop();

  iResult.ResizeTo(1);
  iResult[0]='\0';
  env.iError.ResizeTo(1);
  env.iError[0]='\0';
 
    LispPtr result;
    LispTrap(
     {
         LispPtr lispexpr;
//printf("Input: [%s]\n",aExpression);
         if (env.PrettyReader())
         {
            LispString * prettyReader = env.PrettyReader();
            LispString full(const_cast<LispChar *>(aExpression));  // TODO: woof
            full[full.Size()-1] = ';';
            full.Append('\0');
            StringInput input(full,env.iInputStatus);
            LispLocalInput localInput(env, &input);
            LispPtr args(NULL);
            InternalApplyString(env, lispexpr,
                               prettyReader,
                               args);
         }
         else
         {
           LispString full((LispChar *)aExpression);
           full[full.Size()-1] = ';';
           full.Append('\0');
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
         env.SetVariable(percent,result,LispTrue);
     },env.iErrorOutput,env);
     env.iStack.PopTo(stackTop);
}

LispChar * CYacas::Result()
{
  return iResult.c_str();
}

LispChar * CYacas::Error()
{
  LispEnvironment& env = environment.getEnv();
  return env.iError.c_str();
}



