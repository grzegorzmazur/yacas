
#include "yacasprivate.h"
#include "yacasbase.h"
#include "yacas.h"
#include "mathcommands.h"
#include "standard.h"

#ifdef YACAS_DEBUG
long theNrDefinedBuiltIn=0;
long theNrDefinedUser=0;
#endif


#define InternalEval environment().iEvaluator->Eval

#define OPERATOR(oper_type,prec,name) \
	oper_type##operators.SetOperator(prec,hash.LookUp(#name))
// for example: OPERATOR(bodied,KMaxPrecedence,While) produces:
//    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("While"));


DefaultYacasEnvironment::~DefaultYacasEnvironment()
{
    delete output;
}



DefaultYacasEnvironment::DefaultYacasEnvironment(LispOutput* aOutput)
:output(aOutput),infixprinter(prefixoperators,
             infixoperators,
             postfixoperators,
              bodiedoperators),
iEnvironment(coreCommands,userFunctions,
                 globals,hash,output,infixprinter,
                 prefixoperators,infixoperators,
                 postfixoperators,bodiedoperators,&input),
input(iEnvironment.iInputStatus)
{
    // Define the buitl-in functions by tying their string representation
    // to a kernel callable routine.

#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) iEnvironment.SetCommand(fname,iname,nrargs,flags);
#define CORE_KERNEL_FUNCTION_ALIAS(iname,fname,nrargs,flags) iEnvironment.SetCommand(fname,iname,nrargs,flags);
#include "corefunctions.h"
#undef CORE_KERNEL_FUNCTION
#undef CORE_KERNEL_FUNCTION_ALIAS
    
    OPERATOR(bodied,KMaxPrecedence,While);
    OPERATOR(bodied,KMaxPrecedence,Rule);
    OPERATOR(bodied,KMaxPrecedence,MacroRule);
    OPERATOR(bodied,KMaxPrecedence,RulePattern);
    OPERATOR(bodied,KMaxPrecedence,MacroRulePattern);
    OPERATOR(bodied,KMaxPrecedence,FromFile);
    OPERATOR(bodied,KMaxPrecedence,FromString);
    OPERATOR(bodied,KMaxPrecedence,ToFile);
    OPERATOR(bodied,KMaxPrecedence,ToString);
    OPERATOR(bodied,KMaxPrecedence,TraceRule);
    OPERATOR(bodied,KMaxPrecedence,Subst);
    OPERATOR(bodied,KMaxPrecedence,LocalSymbols);
    OPERATOR(bodied,KMaxPrecedence,BackQuote);
    OPERATOR(prefix,0,`);
    OPERATOR(prefix,0,@);
    OPERATOR(prefix,0,_);
    OPERATOR(infix,0,_);

}






LISPEXPORT CYacas* CYacas::NewL()
{
  CYacas* self = NEW CYacas(NEW StdUserOutput());
  return self;
}
LISPEXPORT CYacas* CYacas::NewL(LispOutput* aOutput)
{
  CYacas* self = NEW CYacas(aOutput);
  return self;
}

LISPEXPORT CYacas::~CYacas()
{
}


CYacas::CYacas(LispOutput* aOutput)
: environment(aOutput),iResultOutput(iResult)
{
}



void CYacas::Evaluate(const LispCharPtr aExpression)
{

  iResult.SetNrItems(1);
  iResult[0]='\0';
  environment().iError.SetNrItems(1);
  environment().iError[0]='\0';
  
    LispPtr result;
    LispTrap(
     {
         LispString full((LispCharPtr)aExpression);
         full[full.NrItems()-1] = ';';
         full.Append('\0');
         StringInput input(full,environment().iInputStatus);
         environment().iInputStatus.SetTo("CommandLine");
         LispPtr lispexpr;
         LispTokenizer &tok = *environment().iCurrentTokenizer;
         InfixParser parser(tok, input,
                            environment(),
                            environment().PreFix(),
                            environment().InFix(),
                            environment().PostFix(),
                            environment().Bodied());
         parser.Parse(lispexpr);

         environment().iEvalDepth=0;
         environment().iEvaluator->ResetStack();
         InternalEval(environment(), result, lispexpr);
         // If no error encountered, print result
         if (environment().PrettyPrinter() != NULL)
         {
             LispPtr nonresult;
             InternalApplyString(environment(), nonresult,
                                 environment().PrettyPrinter(),
                                 result);
         }
         else
         {
             InfixPrinter infixprinter(environment().PreFix(),
                                       environment().InFix(),
                                       environment().PostFix(),
                                       environment().Bodied());

             infixprinter.Print(result, iResultOutput, environment());
             iResultOutput.Write(";");
         }
         LispStringPtr percent = environment().HashTable().LookUp("%");
         environment().SetVariable(percent,result);
         environment().SetGlobalEvaluates(percent);
         
     },environment().iErrorOutput,environment());
     
//printf("stack top = %d (should be zero)\n",environment().iStack.GetStackTop());
     environment().iStack.PopTo(0);
//     LISPASSERT(environment().iStack.GetStackTop() == 0);
}

LispCharPtr CYacas::Result()
{
  return iResult.String();
}

LispCharPtr CYacas::Error()
{
  return environment().iError.String();
}



