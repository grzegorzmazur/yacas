
#include "yacasbase.h"
#include "yacas.h"
#include "mathcommands.h"
#include "standard.h"

#ifdef YACAS_DEBUG
long theNrDefinedBuiltIn=0;
long theNrDefinedUser=0;
#endif


#define InternalEval environment().iEvaluator->Eval


DefaultYacasEnvironment::~DefaultYacasEnvironment()
{
    delete output;
}


void DefaultYacasEnvironment::SetCommand(LispEvalCaller aEvaluatorFunc,
                                         LispCharPtr aString)
{
    iEnvironment.SetCommand(aEvaluatorFunc, aString);
}

DefaultYacasEnvironment::DefaultYacasEnvironment(LispOutput* aOutput)
:output(aOutput),infixprinter(prefixoperators,
             infixoperators,
             postfixoperators,
              bodiedoperators),
iEnvironment(commands,userFunctions,
                 globals,hash,output,infixprinter,
                 prefixoperators,infixoperators,
                 postfixoperators,bodiedoperators,&input),
input(iEnvironment.iInputStatus)
{
    // Define the buitl-in functions by tying their string representation
    // to a kernel callable routine.

    SetCommand(LispQuote         ,"Hold");
    SetCommand(LispEval          ,"Eval");
    SetCommand(LispSetVar        ,"Set");
    SetCommand(LispMacroSetVar   ,"MacroSet");
    SetCommand(LispClearVar      ,"Clear");
    SetCommand(LispMacroClearVar ,"MacroClear");
    SetCommand(LispFullForm      ,"FullForm");

    SetCommand(LispHead          ,"Head");
    SetCommand(LispNth           ,"MathNth");
    SetCommand(LispTail          ,"Tail");
    SetCommand(LispDestructiveReverse       ,"DestructiveReverse");
    SetCommand(LispLength        ,"Length");
    SetCommand(LispList          ,"List");
    SetCommand(LispUnList        ,"UnList");
    SetCommand(LispListify       ,"Listify");
    SetCommand(LispConcatenate   ,"Concat");
    SetCommand(LispConcatenateStrings   ,"ConcatStrings");
    SetCommand(LispDelete        ,"Delete");
    SetCommand(LispInsert        ,"Insert");
    SetCommand(LispReplace       ,"Replace");
    SetCommand(LispNot           ,"MathNot");
    SetCommand(LispNot           ,"Not");
    SetCommand(LispLazyAnd       ,"MathAnd");
    SetCommand(LispLazyAnd       ,"And");
    SetCommand(LispLazyOr        ,"MathOr");
    SetCommand(LispLazyOr        ,"Or");
    SetCommand(LispEquals        ,"Equals");
    SetCommand(LispEquals        ,"=");
    SetCommand(LispWrite         ,"Write");
    /*TODO remove!
    Set*Command(LispSpace         ,"Space");
    Set*Command(LispNewLine       ,"NewLine");
*/
    SetCommand(LispWriteString   ,"WriteString");
    SetCommand(LispProgBody      ,"Prog");
    SetCommand(LispNewLocal      ,"Local");
    SetCommand(LispMacroNewLocal ,"MacroLocal");
    SetCommand(LispWhile         ,"While");
    
    SetCommand(LispMultiply      ,"MathMultiply");

    SetCommand(LispAdd           ,"MathAdd");
    SetCommand(LispSubtract      ,"MathSubtract");
    SetCommand(LispDivide        ,"MathDivide");
    SetCommand(LispDiv           ,"MathDiv");
    SetCommand(LispMod           ,"MathMod");

    SetCommand(LispLessThan      ,"LessThan");
    SetCommand(LispGreaterThan   ,"GreaterThan");
    
    SetCommand(LispPreFix        ,"Prefix");
    SetCommand(LispInFix         ,"Infix");
    SetCommand(LispPostFix       ,"Postfix");
    SetCommand(LispBodied        ,"Bodied");
    SetCommand(LispAtomize       ,"Atom");
    SetCommand(LispStringify     ,"String");
    SetCommand(LispLoad          ,"Load");

    SetCommand(LispRuleBase      ,"RuleBase");
    SetCommand(LispMacroRuleBase ,"MacroRuleBase");
    SetCommand(LispHoldArg       ,"HoldArg");
    SetCommand(LispNewRule       ,"Rule");
    SetCommand(LispMacroNewRule  ,"MacroRule");
    SetCommand(LispUnFence       ,"UnFence");
    SetCommand(LispRetract       ,"Retract");

    SetCommand(LispIsFunction    ,"IsFunction");
    SetCommand(LispIsAtom        ,"IsAtom");
    SetCommand(LispIsNumber      ,"IsNumber");
    SetCommand(LispIsInteger     ,"IsInteger");
    SetCommand(LispIsList        ,"IsList");
    SetCommand(LispIsString      ,"IsString");
    SetCommand(LispIsBound       ,"IsBound");

    SetCommand(LispIf    ,"If");

    SetCommand(LispSin   ,"MathSin");
    SetCommand(LispCos   ,"MathCos");
    SetCommand(LispTan   ,"MathTan");
    SetCommand(LispArcSin   ,"MathArcSin");
    SetCommand(LispArcCos   ,"MathArcCos");
    SetCommand(LispArcTan   ,"MathArcTan");
    SetCommand(LispLog   ,"MathLog");
    SetCommand(LispExp   ,"MathExp");
    SetCommand(LispPower ,"MathPower");

    SetCommand(LispFastSin   ,"FastSin");
    SetCommand(LispFastCos   ,"FastCos");
    SetCommand(LispFastTan   ,"FastTan");
    SetCommand(LispFastArcSin,"FastArcSin");
    SetCommand(LispFastArcCos,"FastArcCos");
    SetCommand(LispFastArcTan,"FastArcTan");
    SetCommand(LispFastLog   ,"FastLog");
    SetCommand(LispFastExp   ,"FastExp");
    SetCommand(LispFastPower ,"FastPower");

    SetCommand(LispPrecision ,"Precision");

    SetCommand(LispSqrt   ,"MathSqrt");
    SetCommand(LispFloor  ,"MathFloor");
    SetCommand(LispCeil   ,"MathCeil");
    SetCommand(LispAbs   ,"MathAbs");
    SetCommand(LispMod   ,"MathMod");
    SetCommand(LispDiv   ,"MathDiv");
    SetCommand(LispPi    ,"MathPi");
    SetCommand(LispGcd   ,"MathGcd");


    SetCommand(LispDefaultDirectory   ,"DefaultDirectory");
    SetCommand(LispFromFile   ,"FromFile");
    SetCommand(LispFromString ,"FromString");
    SetCommand(LispToFile   ,"ToFile");
    SetCommand(LispToString ,"ToString");
    SetCommand(LispRead   ,"Read");
    SetCommand(LispReadToken   ,"ReadToken");
    SetCommand(LispDestructiveDelete,"DestructiveDelete");
    SetCommand(LispDestructiveInsert,"DestructiveInsert");
    SetCommand(LispDestructiveReplace,"DestructiveReplace");
    SetCommand(LispFlatCopy       ,"FlatCopy");

    SetCommand(LispCheck       ,"Check");

    SetCommand(LispSystemCall  ,"SystemCall");


    SetCommand(LispShiftLeft       ,"ShiftLeft");
    SetCommand(LispShiftRight       ,"ShiftRight");
    SetCommand(LispFromBase       ,"FromBase");
    SetCommand(LispToBase       ,"ToBase");

    SetCommand(LispMaxEvalDepth   ,"MaxEvalDepth");

    SetCommand(LispDefLoad   ,"DefLoad");
    SetCommand(LispUse   ,"Use");

    SetCommand(LispRightAssociative   ,"RightAssociative");
    SetCommand(LispLeftPrecedence     ,"LeftPrecedence");
    SetCommand(LispRightPrecedence    ,"RightPrecedence");

    SetCommand(LispIsBodied      ,"IsBodied");
    SetCommand(LispIsInFix      ,"IsInfix");
    SetCommand(LispIsPreFix     ,"IsPrefix");
    SetCommand(LispIsPostFix    ,"IsPostfix");
    SetCommand(LispGetPrecedence,"OpPrecedence");
    SetCommand(LispGetLeftPrecedence,"OpLeftPrecedence");
    SetCommand(LispGetRightPrecedence,"OpRightPrecedence");
    SetCommand(LispGetPrecision ,"GetPrecision");

    SetCommand(LispBitAnd,"BitAnd");
    SetCommand(LispBitOr ,"BitOr");
    SetCommand(LispBitXor,"BitXor");

    SetCommand(LispSecure,"Secure");
    SetCommand(LispFindFile,"FindFile");
    SetCommand(LispFindFunction,"FindFunction");
    
    SetCommand(LispIsGeneric,"IsGeneric");
    SetCommand(LispGenericTypeName,"GenericTypeName");

    SetCommand(GenArrayCreate,"ArrayCreate");
    SetCommand(GenArraySize,"ArraySize");
    SetCommand(GenArrayGet,"ArrayGet");
    SetCommand(GenArraySet,"ArraySet");

    SetCommand(LispTrace,"TraceExp");
    SetCommand(LispTraceRule,"TraceRule");
    SetCommand(LispTraceStack,"TraceStack");
    SetCommand(LispReadLisp,"LispRead");
    SetCommand(LispType,"Type");

    SetCommand(LispStringMid,"StringMid");
    SetCommand(LispSetStringMid,"SetStringMid");

    SetCommand(GenPatternCreate,"PatternCreate");
    SetCommand(GenPatternMatches,"PatternMatches");

    SetCommand(LispRuleBaseDefined,"RuleBaseDefined");
    SetCommand(LispRuleBaseArgList,"RuleBaseArgList");

    SetCommand(LispNewRulePattern       ,"RulePattern");
    SetCommand(LispMacroNewRulePattern  ,"MacroRulePattern");

    SetCommand(LispSubst ,"Subst");
    SetCommand(LispLocalSymbols ,"LocalSymbols");

    SetCommand(LispFac ,"MathFac");
    SetCommand(LispApplyPure ,"ApplyPure");

    SetCommand(LispPrettyPrinter,"PrettyPrinter");

    SetCommand(LispGarbageCollect,"GarbageCollect");
    SetCommand(LispLazyGlobal,"LazyGlobal");

    SetCommand(LispPatchLoad,"PatchLoad");
    SetCommand(LispPatchString,"PatchString");
    SetCommand(LispDllLoad,"DllLoad");
    SetCommand(LispDllUnload,"DllUnload");
    SetCommand(LispDllEnumerate,"DllEnumerate");

    SetCommand(LispSetExtraInfo,"SetExtraInfo");
    SetCommand(LispGetExtraInfo,"GetExtraInfo");

    SetCommand(LispBerlekamp,"Berlekamp");

    SetCommand(LispDefaultTokenizer,"DefaultTokenizer");
    SetCommand(LispCTokenizer      ,"CTokenizer");
    SetCommand(LispXmlTokenizer    ,"XmlTokenizer");

    SetCommand(LispExplodeTag    ,"XmlExplodeTag");

    SetCommand(LispFastAssoc    ,"FastAssoc");

    SetCommand(LispCurrentFile ,"CurrentFile");
    SetCommand(LispCurrentLine ,"CurrentLine");

    SetCommand(LispBackQuote ,"`");
    
    
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("While"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("Rule"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("MacroRule"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("RulePattern"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("MacroRulePattern"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("FromFile"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("FromString"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("ToFile"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("ToString"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("TraceRule"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("Subst"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("LocalSymbols"));
    bodiedoperators.SetOperator(KMaxPrecedence,hash.LookUp("BackQuote"));

    prefixoperators.SetOperator(0,hash.LookUp("`"));
    prefixoperators.SetOperator(0,hash.LookUp("@"));
    prefixoperators.SetOperator(0,hash.LookUp("_"));
    infixoperators.SetOperator(0,hash.LookUp("_"));
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
                            environment().HashTable(),
                            environment().PreFix(),
                            environment().InFix(),
                            environment().PostFix(),
                            environment().Bodied());
         parser.Parse(lispexpr,environment());

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

}

LispCharPtr CYacas::Result()
{
  return iResult.String();
}

LispCharPtr CYacas::Error()
{
  return environment().iError.String();
}



