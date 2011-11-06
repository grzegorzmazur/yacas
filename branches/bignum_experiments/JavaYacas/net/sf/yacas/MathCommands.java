package net.sf.yacas;


import java.io.*;

class MathCommands
{
  public void AddCommands(LispEnvironment aEnvironment)
  {
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"While");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"Rule");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"MacroRule");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"RulePattern");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"MacroRulePattern");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"FromFile");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"FromString");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"ToFile");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"ToString");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"ToStdout");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"TraceRule");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"Subst");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"LocalSymbols");
    aEnvironment.iBodiedOperators.SetOperator(InfixPrinter.KMaxPrecedence,"BackQuote");
    aEnvironment.iPrefixOperators.SetOperator(0,"`");
    aEnvironment.iPrefixOperators.SetOperator(0,"@");
    aEnvironment.iPrefixOperators.SetOperator(0,"_");
    aEnvironment.iInfixOperators.SetOperator(0,"_");

    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispQuote(),1, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "Hold");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispEval(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Eval");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispWrite(),1, YacasEvaluator.Variable|YacasEvaluator.Function),
         "Write");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispWriteString(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "WriteString");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFullForm(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "FullForm");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDefaultDirectory(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DefaultDirectory");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFromFile(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "FromFile");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFromString(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "FromString");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispRead(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Read");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispReadToken(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "ReadToken");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispToFile(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "ToFile");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispToString(),1, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "ToString");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispToStdout(),1, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "ToStdout");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispLoad(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Load");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispSetVar(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "Set");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMacroSetVar(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "MacroSet");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispClearVar(),1, YacasEvaluator.Variable|YacasEvaluator.Macro),
         "Clear");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispClearVar(),1, YacasEvaluator.Variable|YacasEvaluator.Function),
         "MacroClear");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispNewLocal(),1, YacasEvaluator.Variable|YacasEvaluator.Macro),
         "Local");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispNewLocal(),1, YacasEvaluator.Variable|YacasEvaluator.Function),
         "MacroLocal");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispHead(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Head");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispNth(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathNth");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispTail(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Tail");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDestructiveReverse(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DestructiveReverse");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispLength(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Length");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispList(),1, YacasEvaluator.Variable|YacasEvaluator.Macro),
         "List");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispUnList(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "UnList");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispListify(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Listify");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispConcatenate(),1, YacasEvaluator.Variable|YacasEvaluator.Function),
         "Concat");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispConcatenateStrings(),1, YacasEvaluator.Variable|YacasEvaluator.Function),
         "ConcatStrings");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDelete(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Delete");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDestructiveDelete(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DestructiveDelete");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispInsert(),3, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Insert");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDestructiveInsert(),3, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DestructiveInsert");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispReplace(),3, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Replace");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDestructiveReplace(),3, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DestructiveReplace");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispAtomize(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Atom");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispStringify(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "String");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCharString(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "CharString");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFlatCopy(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "FlatCopy");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispProgBody(),1, YacasEvaluator.Variable|YacasEvaluator.Macro),
         "Prog");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispWhile(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "While");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIf(),2, YacasEvaluator.Variable|YacasEvaluator.Macro),
         "If");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCheck(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "Check");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispTrapError(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "TrapError");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispGetCoreError(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "GetCoreError");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispPreFix(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Prefix");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispInFix(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Infix");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispPostFix(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Postfix");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispBodied(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Bodied");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispRuleBase(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "RuleBase");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMacroRuleBase(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MacroRuleBase");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispRuleBaseListed(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "RuleBaseListed");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMacroRuleBaseListed(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MacroRuleBaseListed");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDefMacroRuleBase(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "DefMacroRuleBase");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDefMacroRuleBaseListed(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "DefMacroRuleBaseListed");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispHoldArg(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "HoldArg");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispNewRule(),5, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "Rule");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMacroNewRule(),5, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MacroRule");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispUnFence(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "UnFence");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispRetract(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Retract");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispNot(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathNot");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispNot(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Not");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispLazyAnd(),1, YacasEvaluator.Variable|YacasEvaluator.Macro),
         "MathAnd");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispLazyAnd(),1, YacasEvaluator.Variable|YacasEvaluator.Macro),
         "And");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispLazyOr(),1, YacasEvaluator.Variable|YacasEvaluator.Macro),
         "MathOr");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispLazyOr(),1, YacasEvaluator.Variable|YacasEvaluator.Macro),
         "Or");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispEquals(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Equals");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispEquals(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "=");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispLessThan(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "LessThan");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispGreaterThan(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "GreaterThan");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsFunction(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsFunction");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsAtom(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsAtom");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsNumber(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsNumber");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsInteger(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsInteger");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsList(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsList");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsString(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsString");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsBound(),1, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "IsBound");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMultiply(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathMultiply");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispAdd(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathAdd");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispSubtract(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathSubtract");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDivide(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathDivide");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasBuiltinPrecisionSet(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Builtin'Precision'Set");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispGetExactBits(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathGetExactBits");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispSetExactBits(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathSetExactBits");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispBitCount(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathBitCount");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMathSign(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathSign");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMathIsSmall(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathIsSmall");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMathNegate(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathNegate");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFloor(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathFloor");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCeil(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathCeil");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispAbs(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathAbs");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMod(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathMod");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDiv(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathDiv");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispBitsToDigits(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "BitsToDigits");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDigitsToBits(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DigitsToBits");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispGcd(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathGcd");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispSystemCall(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "SystemCall");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFastArcSin(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "FastArcSin");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFastLog(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "FastLog");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFastPower(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "FastPower");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispShiftLeft(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "ShiftLeft");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispShiftRight(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "ShiftRight");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFromBase(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "FromBase");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispToBase(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "ToBase");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMaxEvalDepth(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MaxEvalDepth");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDefLoad(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DefLoad");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispUse(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Use");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispRightAssociative(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "RightAssociative");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispLeftPrecedence(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "LeftPrecedence");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispRightPrecedence(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "RightPrecedence");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsBodied(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsBodied");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsInFix(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsInfix");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsPreFix(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsPrefix");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsPostFix(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsPostfix");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispGetPrecedence(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "OpPrecedence");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispGetLeftPrecedence(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "OpLeftPrecedence");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispGetRightPrecedence(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "OpRightPrecedence");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasBuiltinPrecisionGet(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Builtin'Precision'Get");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispBitAnd(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "BitAnd");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispBitOr(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "BitOr");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispBitXor(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "BitXor");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispSecure(),1, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "Secure");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFindFile(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "FindFile");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFindFunction(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "FindFunction");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsGeneric(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsGeneric");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispGenericTypeName(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "GenericTypeName");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new GenArrayCreate(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Array'Create");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new GenArraySize(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Array'Size");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new GenArrayGet(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Array'Get");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new GenArraySet(),3, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Array'Set");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCustomEval(),4, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "CustomEval");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCustomEvalExpression(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "CustomEval'Expression");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCustomEvalResult(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "CustomEval'Result");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCustomEvalLocals(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "CustomEval'Locals");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCustomEvalStop(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "CustomEval'Stop");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispTraceRule(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "TraceRule");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispTraceStack(),1, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "TraceStack");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispReadLisp(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "LispRead");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispReadLispListed(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "LispReadListed");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispType(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Type");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasStringMidGet(),3, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "StringMid'Get");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasStringMidSet(),3, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "StringMid'Set");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new GenPatternCreate(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Pattern'Create");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new GenPatternMatches(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Pattern'Matches");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispRuleBaseDefined(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "RuleBaseDefined");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDefLoadFunction(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DefLoadFunction");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispRuleBaseArgList(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "RuleBaseArgList");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispNewRulePattern(),5, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "RulePattern");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispMacroNewRulePattern(),5, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MacroRulePattern");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispSubst(),3, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Subst");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispLocalSymbols(),1, YacasEvaluator.Variable|YacasEvaluator.Macro),
         "LocalSymbols");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFastIsPrime(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "FastIsPrime");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFac(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathFac");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispApplyPure(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "ApplyPure");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasPrettyReaderSet(),1, YacasEvaluator.Variable|YacasEvaluator.Function),
         "PrettyReader'Set");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasPrettyPrinterSet(),1, YacasEvaluator.Variable|YacasEvaluator.Function),
         "PrettyPrinter'Set");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasPrettyPrinterGet(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "PrettyPrinter'Get");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasPrettyReaderGet(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "PrettyReader'Get");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispGarbageCollect(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "GarbageCollect");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispSetGlobalLazyVariable(),2, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "SetGlobalLazyVariable");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispPatchLoad(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "PatchLoad");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispPatchString(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "PatchString");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasExtraInfoSet(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "ExtraInfo'Set");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasExtraInfoGet(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "ExtraInfo'Get");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDefaultTokenizer(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DefaultTokenizer");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCommonLispTokenizer(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "CommonLispTokenizer");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispXmlTokenizer(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "XmlTokenizer");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispExplodeTag(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "XmlExplodeTag");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new YacasBuiltinAssoc(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Builtin'Assoc");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCurrentFile(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "CurrentFile");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispCurrentLine(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "CurrentLine");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispBackQuote(),1, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "`");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDumpBigNumberDebugInfo(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathDebugInfo");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispInDebugMode(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "InDebugMode");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDebugFile(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DebugFile");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispDebugLine(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "DebugLine");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispVersion(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Version");

    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispExit(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "Exit");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispExitRequested(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsExitRequested");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispHistorySize(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "HistorySize");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispStackSize(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "StaSiz");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispIsPromptShown(),0, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "IsPromptShown");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispReadCmdLineString(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "ReadCmdLineString");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispTime(),1, YacasEvaluator.Fixed|YacasEvaluator.Macro),
         "GetTime");
    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispFileSize(),1, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "FileSize");


  }


  /// Construct a BigNumber from one of the arguments.
  /// \param x (on output) the constructed bignumber
  /// \param aEnvironment the current environment
  /// \param aStackTop the index of the top of the stack
  /// \param aArgNr the index of the argument to be converted
  public static BigNumber GetNumber(LispEnvironment aEnvironment, int aStackTop, int aArgNr) throws Exception
  {
    BigNumber x = YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, aArgNr).Get().Number(aEnvironment.Precision());
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,x != null,aArgNr);
    return x;
  }

  static void MultiFix(LispEnvironment aEnvironment, int aStackTop, LispOperators aOps) throws Exception
  {
    // Get operator
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
    String orig = YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
 
    LispPtr precedence = new LispPtr();
    aEnvironment.iEvaluator.Eval(aEnvironment, precedence, YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 2));
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,precedence.Get().String() != null, 2);
    int prec = Integer.parseInt(precedence.Get().String(),10);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,prec <= InfixPrinter.KMaxPrecedence, 2);
    aOps.SetOperator(prec,LispStandard.SymbolName(aEnvironment,orig));
    LispStandard.InternalTrue(aEnvironment,YacasEvalCaller.RESULT(aEnvironment, aStackTop));
  }
  public static void SingleFix(int aPrecedence, LispEnvironment aEnvironment, int aStackTop, LispOperators aOps) throws Exception
  {
    // Get operator
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
    String orig = YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    aOps.SetOperator(aPrecedence,LispStandard.SymbolName(aEnvironment,orig));
    LispStandard.InternalTrue(aEnvironment,YacasEvalCaller.RESULT(aEnvironment, aStackTop));
  }

  public static LispInFixOperator OperatorInfo(LispEnvironment aEnvironment,int aStackTop, LispOperators aOperators) throws Exception
  {
    // Get operator
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);

    LispPtr evaluated = new LispPtr();
    evaluated.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get());

    String orig = evaluated.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    //
    LispInFixOperator op = (LispInFixOperator)aOperators.LookUp(LispStandard.SymbolName(aEnvironment,orig));
    return op;
  }

  /// Execute the Yacas commands \c Set and \c MacroSet.
  /// The argument \a aMacroMode determines whether the first argument
  /// should be evaluated. The real work is done by
  /// LispEnvironment::SetVariable() .
  /// \sa LispSetVar(), LispMacroSetVar()
  static void InternalSetVar(LispEnvironment aEnvironment, int aStackTop, boolean aMacroMode, boolean aGlobalLazyVariable) throws Exception
  {
    String varstring=null;
    if (aMacroMode)
    {
      LispPtr result = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, result, YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1));
      varstring = result.Get().String();
    }
    else
    {
      varstring = YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    }
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,varstring != null,1);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,!LispStandard.IsNumber(varstring,true),1);
 
    LispPtr result = new LispPtr();
    aEnvironment.iEvaluator.Eval(aEnvironment, result, YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 2));
    aEnvironment.SetVariable(varstring, result, aGlobalLazyVariable);
    LispStandard.InternalTrue(aEnvironment,YacasEvalCaller.RESULT(aEnvironment, aStackTop));
  }

  public static void InternalDelete(LispEnvironment aEnvironment, int aStackTop, boolean aDestructive) throws Exception
  {
    LispPtr evaluated = new LispPtr();
    evaluated.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get());
    LispError.CHK_ISLIST_CORE(aEnvironment,aStackTop,evaluated,1);

    LispPtr copied = new LispPtr();
    if (aDestructive)
    {
      copied.Set(evaluated.Get().SubList().Get());
    }
    else
    {
      LispStandard.InternalFlatCopy(copied,evaluated.Get().SubList());
    }

    LispPtr index = new LispPtr();
    index.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 2).Get());
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get() != null, 2);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get().String() != null, 2);
    int ind = Integer.parseInt(index.Get().String(),10);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ind>0,2);

    LispIterator iter = new LispIterator(copied);
    while (ind>0)
    {
      iter.GoNext();
      ind--;
    }
    LispError.CHK_CORE(aEnvironment, aStackTop,iter.GetObject() != null, LispError.KLispErrListNotLongEnough);
    LispPtr next = new LispPtr();
    next.Set(iter.GetObject().Next().Get());
    iter.Ptr().Set(next.Get());
    YacasEvalCaller.RESULT(aEnvironment, aStackTop).Set(LispSubList.New(copied.Get()));
  }


  public static void InternalInsert(LispEnvironment aEnvironment, int aStackTop, boolean aDestructive) throws Exception
  {
    LispPtr evaluated = new LispPtr();
    evaluated.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get());
    LispError.CHK_ISLIST_CORE(aEnvironment,aStackTop,evaluated,1);

    LispPtr copied = new LispPtr();
    if (aDestructive)
    {
        copied.Set(evaluated.Get().SubList().Get());
    }
    else
    {
        LispStandard.InternalFlatCopy(copied,evaluated.Get().SubList());
    }
 
    LispPtr index = new LispPtr();
    index.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 2).Get());
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get() != null, 2);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get().String() != null, 2);
    int ind = Integer.parseInt(index.Get().String(),10);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ind>0,2);

    LispIterator iter = new LispIterator(copied);
    while (ind>0)
    {
      iter.GoNext();
      ind--;
    }

    LispPtr toInsert = new LispPtr();
    toInsert.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 3).Get());
    toInsert.Get().Next().Set(iter.GetObject());
    iter.Ptr().Set(toInsert.Get());
    YacasEvalCaller.RESULT(aEnvironment, aStackTop).Set(LispSubList.New(copied.Get()));
  }






  public static void InternalReplace(LispEnvironment aEnvironment, int aStackTop, boolean aDestructive) throws Exception
  {
    LispPtr evaluated = new LispPtr();
    evaluated.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get());
    // Ok, so lets not check if it is a list, but it needs to be at least a 'function'
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get().SubList() != null, 1);

    LispPtr index = new LispPtr();
    index.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 2).Get());
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get() != null, 2);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get().String() != null, 2);
    int ind = Integer.parseInt(index.Get().String(),10);

    LispPtr copied = new LispPtr();
    if (aDestructive)
    {
      copied.Set(evaluated.Get().SubList().Get());
    }
    else
    {
      LispStandard.InternalFlatCopy(copied,evaluated.Get().SubList());
    }
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ind>0,2);

    LispIterator iter = new LispIterator(copied);
    while (ind>0)
    {
      iter.GoNext();
      ind--;
    }

    LispPtr toInsert = new LispPtr();
    toInsert.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 3).Get());
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,iter.Ptr() != null, 2);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,iter.Ptr().Get() != null, 2);
    toInsert.Get().Next().Set(iter.Ptr().Get().Next().Get());
    iter.Ptr().Set(toInsert.Get());
    YacasEvalCaller.RESULT(aEnvironment, aStackTop).Set(LispSubList.New(copied.Get()));
  }


  /// Implements the Yacas functions \c RuleBase and \c MacroRuleBase .
  /// The real work is done by LispEnvironment::DeclareRuleBase().
  public static void InternalRuleBase(LispEnvironment aEnvironment, int aStackTop,  boolean aListed) throws Exception
  {
    //TESTARGS(3);
 
    // Get operator
    LispPtr args = new LispPtr();
    String orig=null;
 
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
    orig = YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    args.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 2).Get());
 
    // The arguments
    LispError.CHK_ISLIST_CORE(aEnvironment,aStackTop,args,2);

    // Finally define the rule base
    aEnvironment.DeclareRuleBase(LispStandard.SymbolName(aEnvironment,orig),
                                 args.Get().SubList().Get().Next(),aListed);
 
    // Return true
    LispStandard.InternalTrue(aEnvironment,YacasEvalCaller.RESULT(aEnvironment, aStackTop));
  }

  public static void InternalNewRule(LispEnvironment aEnvironment, int aStackTop) throws Exception
  {
    //TESTARGS(6);

    int arity;
    int precedence;

    LispPtr ar = new LispPtr();
    LispPtr pr = new LispPtr();
    LispPtr predicate = new LispPtr();
    LispPtr body = new LispPtr();
    String orig=null;
 
    // Get operator
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
    orig = YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    ar.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 2).Get());
    pr.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 3).Get());
    predicate.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 4).Get());
    body.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 5).Get());
 
    // The arity
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ar.Get() != null, 2);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ar.Get().String() != null, 2);
    arity = Integer.parseInt(ar.Get().String(),10);

    // The precedence
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,pr.Get() != null, 3);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,pr.Get().String() != null, 3);
    precedence = Integer.parseInt(pr.Get().String(),10);
 
    // Finally define the rule base
    aEnvironment.DefineRule(LispStandard.SymbolName(aEnvironment,orig),
                            arity,
                            precedence,
                            predicate,
                            body );

    // Return true
    LispStandard.InternalTrue(aEnvironment,YacasEvalCaller.RESULT(aEnvironment, aStackTop));
  }




  void InternalDefMacroRuleBase(LispEnvironment aEnvironment, int aStackTop, boolean aListed) throws Exception
  {
    // Get operator
    LispPtr args = new LispPtr();
    LispPtr body = new LispPtr();
    String orig=null;
 
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
    orig = YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

    // The arguments
    args.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 2).Get());
    LispError.CHK_ISLIST_CORE(aEnvironment,aStackTop,args,2);

    // Finally define the rule base
    aEnvironment.DeclareMacroRuleBase(LispStandard.SymbolName(aEnvironment,orig),
                                 args.Get().SubList().Get().Next(),aListed);
 
    // Return true
    LispStandard.InternalTrue(aEnvironment,YacasEvalCaller.RESULT(aEnvironment, aStackTop));
  }




  public void InternalNewRulePattern(LispEnvironment aEnvironment, int aStackTop, boolean aMacroMode) throws Exception
  {
    int arity;
    int precedence;

    LispPtr ar = new LispPtr();
    LispPtr pr = new LispPtr();
    LispPtr predicate = new LispPtr();
    LispPtr body = new LispPtr();
    String orig=null;
 
    // Get operator
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
    orig = YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    ar.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 2).Get());
    pr.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 3).Get());
    predicate.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 4).Get());
    body.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 5).Get());
 
    // The arity
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ar.Get() != null, 2);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ar.Get().String() != null, 2);
    arity = Integer.parseInt(ar.Get().String(),10);

    // The precedence
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,pr.Get() != null, 3);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,pr.Get().String() != null, 3);
    precedence = Integer.parseInt(pr.Get().String(),10);
 
    // Finally define the rule base
    aEnvironment.DefineRulePattern(LispStandard.SymbolName(aEnvironment,orig),
                            arity,
                            precedence,
                            predicate,
                            body );

    // Return true
    LispStandard.InternalTrue(aEnvironment,YacasEvalCaller.RESULT(aEnvironment, aStackTop));
  }



  class LispQuote extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      RESULT(aEnvironment, aStackTop).Set(ARGUMENT(aEnvironment, aStackTop, 1).Get().Copy(false));
    }
  }

  class LispEval extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 1));
    }
  }

  class LispWrite extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr subList = ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList();
      if (subList != null)
      {
        LispIterator iter = new LispIterator(subList);
        iter.GoNext();
        while (iter.GetObject() != null)
        {
          aEnvironment.iCurrentPrinter.Print(iter.Ptr(),aEnvironment.iCurrentOutput,aEnvironment);
          iter.GoNext();
        }
      }
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispWriteString extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get()!= null,1);
      String str = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str != null,1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str.charAt(0) == '\"',1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str.charAt(str.length()-1) == '\"',1);

      int i=1;
      int nr=str.length()-1;
      //((*str)[i] != '\"')
      for (i=1;i<nr;i++)
      {
        aEnvironment.iCurrentOutput.PutChar(str.charAt(i));
      }
      // pass last printed character to the current printer
      aEnvironment.iCurrentPrinter.RememberLastChar(str.charAt(nr-1));  // hacky hacky
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispFullForm extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      RESULT(aEnvironment, aStackTop).Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispPrinter printer = new LispPrinter();
      printer.Print(RESULT(aEnvironment, aStackTop), aEnvironment.iCurrentOutput, aEnvironment);
      aEnvironment.iCurrentOutput.Write("\n");
    }
  }

  class LispDefaultDirectory extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // Get file name
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
      String orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      String oper = LispStandard.InternalUnstringify(orig);
      aEnvironment.iInputDirectories.add(oper);
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispFromFile extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_CORE(aEnvironment, aStackTop,aEnvironment.iSecure == false, LispError.KLispErrSecurityBreach);
      LispPtr evaluated = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, evaluated, ARGUMENT(aEnvironment, aStackTop, 1));

      // Get file name
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

      String hashedname = aEnvironment.HashTable().LookUpUnStringify(orig);

      InputStatus oldstatus = aEnvironment.iInputStatus;
      LispInput previous = aEnvironment.iCurrentInput;
      try
      {
        aEnvironment.iInputStatus.SetTo(hashedname);
        LispInput input = // new StdFileInput(hashedname, aEnvironment.iInputStatus);
          LispStandard.OpenInputFile(aEnvironment, aEnvironment.iInputDirectories, hashedname, aEnvironment.iInputStatus);
        aEnvironment.iCurrentInput = input;
        // Open file
        LispError.CHK_CORE(aEnvironment, aStackTop,input != null, LispError.KLispErrFileNotFound);

        // Evaluate the body
        aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 2));
      }
      catch (Exception e)
      {
        throw e;
      }
      finally
      {
        aEnvironment.iCurrentInput = previous;
        aEnvironment.iInputStatus.RestoreFrom(oldstatus);
      }
      //Return the result
    }
  }

  class LispFromString extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, evaluated, ARGUMENT(aEnvironment, aStackTop, 1));

      // Get file name
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      String oper = LispStandard.InternalUnstringify(orig);

      InputStatus oldstatus = aEnvironment.iInputStatus;
      aEnvironment.iInputStatus.SetTo("String");
      StringInput newInput = new StringInput(new StringBuffer(oper),aEnvironment.iInputStatus);

      LispInput previous = aEnvironment.iCurrentInput;
      aEnvironment.iCurrentInput = newInput;
      try
      {
        // Evaluate the body
        aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 2));
      }
      catch (Exception e)
      {
        throw e;
      }
      finally
      {
        aEnvironment.iCurrentInput = previous;
        aEnvironment.iInputStatus.RestoreFrom(oldstatus);
      }

      //Return the result
    }
  }

  class LispRead extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InfixParser parser = new InfixParser(aEnvironment.iCurrentTokenizer,
                         aEnvironment.iCurrentInput,
                         aEnvironment,
                         aEnvironment.iPrefixOperators,
                         aEnvironment.iInfixOperators,
                         aEnvironment.iPostfixOperators,
                         aEnvironment.iBodiedOperators);
      // Read expression
      parser.Parse(RESULT(aEnvironment, aStackTop));
    }
  }

  class LispReadToken extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispTokenizer tok = aEnvironment.iCurrentTokenizer;
      String result;
      result = tok.NextToken(aEnvironment.iCurrentInput, aEnvironment.HashTable());

      if (result.length() == 0)
      {
          RESULT(aEnvironment, aStackTop).Set(aEnvironment.iEndOfFile.Copy(false));
          return;
      }
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,result));
    }
  }

  class LispToFile extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_CORE(aEnvironment, aStackTop,aEnvironment.iSecure == false, LispError.KLispErrSecurityBreach);

      LispPtr evaluated = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, evaluated, ARGUMENT(aEnvironment, aStackTop, 1));

      // Get file name
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      String oper = LispStandard.InternalUnstringify(orig);

      // Open file for writing
      FileOutputStream localFP = new FileOutputStream(oper);
      LispError.CHK_CORE(aEnvironment, aStackTop,localFP != null, LispError.KLispErrFileNotFound);
      StdFileOutput newOutput = new StdFileOutput(localFP);

      LispOutput previous = aEnvironment.iCurrentOutput;
      aEnvironment.iCurrentOutput = newOutput;

      try
      {
        aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 2));
      }
      catch (Exception e) { throw e; }
      finally
      {
        aEnvironment.iCurrentOutput = previous;
      }
    }
  }

  class LispToString extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      StringBuffer oper = new StringBuffer();
      StringOutput newOutput = new StringOutput(oper);
      LispOutput previous = aEnvironment.iCurrentOutput;
      aEnvironment.iCurrentOutput = newOutput;
      try
      {
        // Evaluate the body
        aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 1));

        //Return the result
        RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpStringify(oper.toString())));
      }
      catch (Exception e) { throw e; }
      finally
      {
        aEnvironment.iCurrentOutput = previous;
      }
    }
  }

  class LispToStdout extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispOutput previous = aEnvironment.iCurrentOutput;
      aEnvironment.iCurrentOutput = aEnvironment.iInitialOutput;
      try
      {
        aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 1));
      }
      catch (Exception e) { throw e; }
      finally
      {
        aEnvironment.iCurrentOutput = previous;
      }
    }
  }

  class LispLoad extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_CORE(aEnvironment, aStackTop,aEnvironment.iSecure == false, LispError.KLispErrSecurityBreach);

      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      // Get file name
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

      LispStandard.InternalLoad(aEnvironment,orig);
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispSetVar extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.InternalSetVar(aEnvironment, aStackTop, false, false);
    }
  }

  class LispMacroSetVar extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.InternalSetVar(aEnvironment, aStackTop, true, false);
    }
  }

  class LispSetGlobalLazyVariable extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.InternalSetVar(aEnvironment, aStackTop, false, true);
    }
  }

  class LispClearVar extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr subList = ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList();
      if (subList != null)
      {
        LispIterator iter = new LispIterator(subList);
        iter.GoNext();
        int nr=1;
        while (iter.GetObject() != null)
        {
          String str;
          str = iter.GetObject().String();
          LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str != null, nr);
          aEnvironment.UnsetVariable(str);
          iter.GoNext();
          nr++;
        }
      }
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispNewLocal extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr subList = ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList();
      if (subList!= null)
      {
        LispIterator iter = new LispIterator(subList);
        iter.GoNext();

        int nr = 1;
        while (iter.GetObject() != null)
        {
          String variable = iter.GetObject().String();
          LispError.CHK_ARG_CORE(aEnvironment,aStackTop,variable != null,nr);
    // printf("Variable %s\n",variable.String());
          aEnvironment.NewLocal(variable,null);
          iter.GoNext();
          nr++;
        }
      }
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispHead extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispStandard.InternalNth(RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 1),1);
    }
  }

  class LispNth extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      String str;
      str = ARGUMENT(aEnvironment, aStackTop, 2).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str != null,2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,LispStandard.IsNumber(str,false),2);
      int index = Integer.parseInt(str);
      LispStandard.InternalNth(RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 1), index);
    }
  }

  class LispTail extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr first = new LispPtr();
      LispStandard.InternalTail(first, ARGUMENT(aEnvironment, aStackTop, 1));
      LispStandard.InternalTail(RESULT(aEnvironment, aStackTop), first);
      LispPtr head = new LispPtr();
      head.Set(aEnvironment.iList.Copy(false));
      head.Get().Next().Set(RESULT(aEnvironment, aStackTop).Get().SubList().Get());
      RESULT(aEnvironment, aStackTop).Get().SubList().Set(head.Get());
    }
  }

  class LispDestructiveReverse extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr reversed = new LispPtr();
      reversed.Set(aEnvironment.iList.Copy(false));
      LispStandard.InternalReverseList(reversed.Get().Next(), ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList().Get().Next());
      RESULT(aEnvironment, aStackTop).Set(LispSubList.New(reversed.Get()));
    }
  }

  class LispLength extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr subList = ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList();
      if (subList != null)
      {
        int num = LispStandard.InternalListLength(subList.Get().Next());
        RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,""+num));
        return;
      }
      String string = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      if (LispStandard.InternalIsString(string))
      {
        int num = string.length()-2;
        RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,""+num));
        return;
      }
      GenericClass gen = ARGUMENT(aEnvironment, aStackTop, 1).Get().Generic();
      if (gen != null)
        if (gen.TypeName().equals("\"Array\""))
        {
          int size=((ArrayClass)gen).Size();
          RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,""+size));
          return;
        }
    //  CHK_ISLIST_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1),1);
    }
  }

  class LispList extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr all = new LispPtr();
      all.Set(aEnvironment.iList.Copy(false));
      LispIterator tail = new LispIterator(all);
      tail.GoNext();
      LispIterator iter = new LispIterator(ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList());
      iter.GoNext();
      while (iter.GetObject() != null)
      {
        LispPtr evaluated = new LispPtr();
        aEnvironment.iEvaluator.Eval(aEnvironment,evaluated,iter.Ptr());
        tail.Ptr().Set(evaluated.Get());
        tail.GoNext();
        iter.GoNext();
      }
      RESULT(aEnvironment, aStackTop).Set(LispSubList.New(all.Get()));
    }
  }

  class LispUnList extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList() != null, 1);
      LispObject subList = ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList().Get();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,subList != null, 1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,subList.String() == aEnvironment.iList.String(),1);
      LispStandard.InternalTail(RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 1));
    }
  }

  class LispListify extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList() != null, 1);
      LispPtr head = new LispPtr();
      head.Set(aEnvironment.iList.Copy(false));
      head.Get().Next().Set(ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList().Get());
      RESULT(aEnvironment, aStackTop).Set(LispSubList.New(head.Get()));
    }
  }

  class LispConcatenate extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr all = new LispPtr();
      all.Set(aEnvironment.iList.Copy(false));
      LispIterator tail = new LispIterator(all);
      tail.GoNext();
      int arg = 1;

      LispIterator iter = new LispIterator(ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList());
      iter.GoNext();
      while (iter.GetObject() != null)
      {
        LispError.CHK_ISLIST_CORE(aEnvironment,aStackTop,iter.Ptr(),arg);
        LispStandard.InternalFlatCopy(tail.Ptr(),iter.Ptr().Get().SubList().Get().Next());
        while (tail.GetObject() != null)
          tail.GoNext();
        iter.GoNext();
        arg++;
      }
      RESULT(aEnvironment, aStackTop).Set(LispSubList.New(all.Get()));
    }
  }

  class LispConcatenateStrings extends YacasEvalCaller
  {
    void ConcatenateStrings(StringBuffer aStringBuffer, LispEnvironment aEnvironment, int aStackTop) throws Exception
    {
      aStringBuffer.append('\"');
      int arg=1;
 
      LispIterator iter = new LispIterator(ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList());
      iter.GoNext();
      while (iter.GetObject() != null)
      {
        LispError.CHK_ISSTRING_CORE(aEnvironment,aStackTop,iter.Ptr(),arg);
        String thisString = iter.GetObject().String();
        String toAppend = thisString.substring(1,thisString.length()-1);
        aStringBuffer.append(toAppend);
        iter.GoNext();
        arg++;
      }
      aStringBuffer.append('\"');
    }
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      StringBuffer strBuffer = new StringBuffer("");
      ConcatenateStrings(strBuffer,aEnvironment, aStackTop);
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,strBuffer.toString()));
    }
  }

  class LispDelete extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.InternalDelete(aEnvironment, aStackTop,false);
    }
  }

  class LispDestructiveDelete extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.InternalDelete(aEnvironment, aStackTop,true);
    }
  }

  class LispInsert extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.InternalInsert(aEnvironment, aStackTop,false);
    }
  }

  class LispDestructiveInsert extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.InternalInsert(aEnvironment, aStackTop,true);
    }
  }

  class LispReplace extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.InternalReplace(aEnvironment, aStackTop,false);
    }
  }

  class LispDestructiveReplace extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.InternalReplace(aEnvironment, aStackTop,true);
    }
  }

  class LispAtomize extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      // Get operator
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpUnStringify(orig)));
    }
  }

  class LispStringify extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      // Get operator
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpStringify(orig)));
    }
  }

  class LispCharString extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      String str;
      str = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str != null,2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,LispStandard.IsNumber(str,false),2);
      char asciiCode = (char)Integer.parseInt(str,10);
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,"\""+asciiCode+"\""));
    }
  }

  class LispFlatCopy extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr copied = new LispPtr();
      LispStandard.InternalFlatCopy(copied,ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList());
      RESULT(aEnvironment, aStackTop).Set(LispSubList.New(copied.Get()));
    }
  }

  class LispProgBody extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // Allow accessing previous locals.
      aEnvironment.PushLocalFrame(false);
      try
      {
        LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
 
        // Evaluate args one by one.

        LispIterator iter = new LispIterator(ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList());
        iter.GoNext();
        while (iter.GetObject() != null)
        {
          aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), iter.Ptr());
          iter.GoNext();
        }
      }
      catch (Exception e) { throw e; }
      finally
      {
        aEnvironment.PopLocalFrame();
      }
    }
  }

  class LispWhile extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr arg1 = ARGUMENT(aEnvironment, aStackTop, 1);
      LispPtr arg2 = ARGUMENT(aEnvironment, aStackTop, 2);
 
      LispPtr predicate = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, predicate, arg1);

      while (LispStandard.IsTrue(aEnvironment,predicate))
      {
          LispPtr evaluated = new LispPtr();
          aEnvironment.iEvaluator.Eval(aEnvironment, evaluated, arg2);
          aEnvironment.iEvaluator.Eval(aEnvironment, predicate, arg1);

      }
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,LispStandard.IsFalse(aEnvironment,predicate),1);
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispIf extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      int nrArguments = LispStandard.InternalListLength(ARGUMENT(aEnvironment, aStackTop, 0));
      LispError.CHK_CORE(aEnvironment,aStackTop,nrArguments == 3 || nrArguments == 4,LispError.KLispErrWrongNumberOfArgs);

      LispPtr predicate = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, predicate, ARGUMENT(aEnvironment, aStackTop, 1));

      if (LispStandard.IsTrue(aEnvironment,predicate))
      {
        aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), Argument(ARGUMENT(aEnvironment, aStackTop, 0),2));
      }
      else
      {
          LispError.CHK_ARG_CORE(aEnvironment,aStackTop,LispStandard.IsFalse(aEnvironment,predicate),1);
          if (nrArguments == 4)
          {
            aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), Argument(ARGUMENT(aEnvironment, aStackTop, 0),3));
          }
          else
          {
            LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));
          }
      }
    }
  }

  class LispCheck extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr pred = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, pred, ARGUMENT(aEnvironment, aStackTop, 1));
      if (!LispStandard.IsTrue(aEnvironment,pred))
      {
          LispPtr evaluated = new LispPtr();
          aEnvironment.iEvaluator.Eval(aEnvironment, evaluated, ARGUMENT(aEnvironment, aStackTop, 2));
          LispError.CHK_ISSTRING_CORE(aEnvironment,aStackTop,evaluated,2);
          throw new Exception(evaluated.Get().String());
      }
      RESULT(aEnvironment, aStackTop).Set(pred.Get());
    }
  }

  class LispTrapError extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      try
      {
        aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 1));
      }
      catch (Exception e)
      {
        aEnvironment.iError = e.toString();
        aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 2));
        aEnvironment.iError = null;
      }
    }
  }

  class LispGetCoreError extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpStringify(aEnvironment.iError)));
    }
  }

  class LispPreFix extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.MultiFix(aEnvironment, aStackTop, aEnvironment.iPrefixOperators);
    }
  }

  class LispInFix extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.MultiFix(aEnvironment, aStackTop, aEnvironment.iInfixOperators);
    }
  }

  class LispPostFix extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      int nrArguments = LispStandard.InternalListLength(ARGUMENT(aEnvironment, aStackTop, 0));
      if (nrArguments == 2)
      {
        MathCommands.SingleFix(0, aEnvironment, aStackTop, aEnvironment.iPostfixOperators);
      }
      else
      {
        MathCommands.MultiFix(aEnvironment, aStackTop, aEnvironment.iPostfixOperators);
      }
    }
  }

  class LispBodied extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      MathCommands.MultiFix(aEnvironment, aStackTop, aEnvironment.iBodiedOperators);
    }
  }

  class LispRuleBase extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InternalRuleBase(aEnvironment, aStackTop, false);
    }
  }

  class LispMacroRuleBase extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InternalRuleBase(aEnvironment, aStackTop, false);
    }
  }

  class LispRuleBaseListed extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InternalRuleBase(aEnvironment, aStackTop, true);
    }
  }

  class LispMacroRuleBaseListed extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InternalRuleBase(aEnvironment, aStackTop, true);
    }
  }

  class LispDefMacroRuleBase extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InternalDefMacroRuleBase(aEnvironment, aStackTop, false);
    }
  }

  class LispDefMacroRuleBaseListed extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InternalDefMacroRuleBase(aEnvironment, aStackTop, true);
    }
  }

  class LispHoldArg extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // Get operator
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
      String orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

      // The arguments
      String tohold = ARGUMENT(aEnvironment, aStackTop, 2).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,tohold != null, 2);
      aEnvironment.HoldArgument(LispStandard.SymbolName(aEnvironment,orig), tohold);
      // Return true
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispNewRule extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InternalNewRule(aEnvironment, aStackTop);
    }
  }

  class LispMacroNewRule extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InternalNewRule(aEnvironment, aStackTop);
    }
  }

  class LispUnFence extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // Get operator
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
      String orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

      // The arity
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 2).Get() != null, 2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 2).Get().String() != null, 2);
      int arity = Integer.parseInt(ARGUMENT(aEnvironment, aStackTop, 2).Get().String(),10);

      aEnvironment.UnFenceRule(LispStandard.SymbolName(aEnvironment,orig), arity);
 
      // Return true
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispRetract extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // Get operator
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      String oper = LispStandard.SymbolName(aEnvironment,orig);
 
      LispPtr arity = new LispPtr();
      arity.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,arity.Get().String() != null, 2);
      int ar = Integer.parseInt(arity.Get().String(),10);
      aEnvironment.Retract(oper, ar);
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispNot extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      if (LispStandard.IsTrue(aEnvironment, evaluated) || LispStandard.IsFalse(aEnvironment, evaluated))
      {
        LispStandard.InternalNot(RESULT(aEnvironment, aStackTop), aEnvironment, evaluated);
      }
      else
      {
        LispPtr ptr = new LispPtr();
        ptr.Set(ARGUMENT(aEnvironment, aStackTop, 0).Get().Copy(false));
        ptr.Get().Next().Set(evaluated.Get());
        RESULT(aEnvironment, aStackTop).Set(LispSubList.New(ptr.Get()));
      }
    }
  }

  class LispLazyAnd extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr nogos = new LispPtr();
      int nrnogos=0;
      LispPtr evaluated = new LispPtr();

      LispIterator iter = new LispIterator(ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList());
      iter.GoNext();
      while (iter.GetObject() != null)
      {
          aEnvironment.iEvaluator.Eval(aEnvironment, evaluated, iter.Ptr());
          if (LispStandard.IsFalse(aEnvironment, evaluated))
          {
              LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));
              return;
          }
          else if (!LispStandard.IsTrue(aEnvironment, evaluated))
          {
              LispPtr ptr = new LispPtr();
              nrnogos++;
              ptr.Set(evaluated.Get().Copy(false));
              ptr.Get().Next().Set(nogos.Get());
              nogos.Set(ptr.Get());
          }
 
          iter.GoNext();
      }

      if (nogos.Get() != null)
      {
          if (nrnogos == 1)
          {
              RESULT(aEnvironment, aStackTop).Set(nogos.Get());
          }
          else
          {
              LispPtr ptr = new LispPtr();

              LispStandard.InternalReverseList(ptr, nogos);
              nogos.Set(ptr.Get());

              ptr.Set(ARGUMENT(aEnvironment, aStackTop, 0).Get().Copy(false));
              ptr.Get().Next().Set(nogos.Get());
              nogos.Set(ptr.Get());
              RESULT(aEnvironment, aStackTop).Set(LispSubList.New(nogos.Get()));

              //aEnvironment.CurrentPrinter().Print(RESULT(aEnvironment, aStackTop), *aEnvironment.CurrentOutput());
          }
      }
      else
      {
          LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
      }
    }
  }

  class LispLazyOr extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr nogos = new LispPtr();
      int nrnogos=0;

      LispPtr evaluated = new LispPtr();

      LispIterator iter = new LispIterator(ARGUMENT(aEnvironment, aStackTop, 1).Get().SubList());
      iter.GoNext();
      while (iter.GetObject() != null)
      {
        aEnvironment.iEvaluator.Eval(aEnvironment, evaluated, iter.Ptr());
        if (LispStandard.IsTrue(aEnvironment, evaluated))
        {
          LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
          return;
        }
        else if (!LispStandard.IsFalse(aEnvironment, evaluated))
        {
          LispPtr ptr = new LispPtr();
          nrnogos++;

          ptr.Set(evaluated.Get().Copy(false));
          ptr.Get().Next().Set(nogos.Get());
          nogos.Set(ptr.Get());
        }
        iter.GoNext();
      }

      if (nogos.Get() != null)
      {
        if (nrnogos == 1)
        {
          RESULT(aEnvironment, aStackTop).Set(nogos.Get());
        }
        else
        {
          LispPtr ptr = new LispPtr();

          LispStandard.InternalReverseList(ptr, nogos);
          nogos.Set(ptr.Get());

          ptr.Set(ARGUMENT(aEnvironment, aStackTop, 0).Get().Copy(false));
          ptr.Get().Next().Set(nogos.Get());
          nogos.Set(ptr.Get());
          RESULT(aEnvironment, aStackTop).Set(LispSubList.New(nogos.Get()));
        }
        //aEnvironment.CurrentPrinter().Print(RESULT(aEnvironment, aStackTop), *aEnvironment.CurrentOutput());
      }
      else
      {
        LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));
      }
    }
  }

  class LispEquals extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated1 = new LispPtr();
      evaluated1.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispPtr evaluated2 = new LispPtr();
      evaluated2.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());

      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop),
                      LispStandard.InternalEquals(aEnvironment, evaluated1, evaluated2));
    }
  }

  abstract class LispLexCompare2
  {
    abstract boolean lexfunc(String f1, String f2, LispHashTable aHashTable,int aPrecision);
    abstract boolean numfunc(BigNumber n1, BigNumber n2);

    void Compare(LispEnvironment aEnvironment, int aStackTop) throws Exception
    {
      LispPtr result1 = new LispPtr();
      LispPtr result2 = new LispPtr();
      result1.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 1).Get());
      result2.Set(YacasEvalCaller.ARGUMENT(aEnvironment, aStackTop, 2).Get());
      boolean cmp;
      BigNumber n1 = result1.Get().Number(aEnvironment.Precision());
      BigNumber n2 = result2.Get().Number(aEnvironment.Precision());
      if (n1 != null && n2 != null)
      {
        cmp =numfunc(n1,n2);
      }
      else
      {
        String str1;
        String str2;
        str1 = result1.Get().String();
        str2 = result2.Get().String();
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str1 != null ,1);
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str2 != null, 2);
      // the precision argument is ignored in "lex" functions
        cmp =lexfunc(str1,str2,
                              aEnvironment.HashTable(),
                              aEnvironment.Precision());
      }
 
      LispStandard.InternalBoolean(aEnvironment,YacasEvalCaller.RESULT(aEnvironment, aStackTop), cmp);
    }
  }


  class LexLessThan extends LispLexCompare2
  {
    boolean lexfunc(String f1, String f2, LispHashTable aHashTable,int aPrecision)
    {
      return f1.compareTo(f2)<0;
    }
    boolean numfunc(BigNumber n1, BigNumber n2)
    {
      return n1.LessThan(n2) && !n1.Equals(n2);
    }
  }
  class LexGreaterThan extends LispLexCompare2
  {
    boolean lexfunc(String f1, String f2, LispHashTable aHashTable,int aPrecision)
    {
      return f1.compareTo(f2)>0;
    }
    boolean numfunc(BigNumber n1, BigNumber n2)
    {
      return !(n1.LessThan(n2) || n1.Equals(n2));
    }
  }
 

  class LispLessThan extends YacasEvalCaller
  {
    LexLessThan compare = new LexLessThan();
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      compare.Compare(aEnvironment,aStackTop);
    }
  }

  class LispGreaterThan extends YacasEvalCaller
  {
    LexGreaterThan compare = new LexGreaterThan();
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      compare.Compare(aEnvironment,aStackTop);
    }
  }

  class LispIsFunction extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr result = new LispPtr();
      result.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop),
                      result.Get().SubList()!=null);
    }
  }

  class LispIsAtom extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr result = new LispPtr();
      result.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      String s = result.Get().String();
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop),s!=null);
    }
  }

  class LispIsNumber extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr result = new LispPtr();
      result.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop), result.Get().Number(aEnvironment.Precision()) != null);
    }
  }

  class LispIsInteger extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr result = new LispPtr();
      result.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      BigNumber num = result.Get().Number(aEnvironment.Precision());
      if (num == null)
      {
        LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));
      }
      else
      {
        LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop), num.IsInt());
      }
    }
  }

  class LispIsList extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr result = new LispPtr();
      result.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop),LispStandard.InternalIsList(result));
    }
  }

  class LispIsString extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr result = new LispPtr();
      result.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop),
                      LispStandard.InternalIsString(result.Get().String()));
    }
  }

  class LispIsBound extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      String str = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      if (str != null)
      {
        LispPtr val = new LispPtr();
        aEnvironment.GetVariable(str,val);
        if (val.Get() != null)
        {
          LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
          return;
        }
      }
      LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispMultiply extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.Multiply(x,y,aEnvironment.Precision());
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

/// Corresponds to the Yacas function \c MathAdd.
/// If called with one argument (unary plus), this argument is
/// converted to BigNumber. If called with two arguments (binary plus),
/// both argument are converted to a BigNumber, and these are added
/// together at the current precision. The sum is returned.
/// \sa GetNumber(), BigNumber::Add()
  class LispAdd extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      int length = LispStandard.InternalListLength(ARGUMENT(aEnvironment, aStackTop, 0));
      if (length == 2)
      {
        BigNumber x;
        x = MathCommands.GetNumber(aEnvironment, aStackTop, 1);
        RESULT(aEnvironment, aStackTop).Set(new LispNumber(x));
        return;
      }
      else
      {
        BigNumber x = MathCommands.GetNumber(aEnvironment, aStackTop, 1);
        BigNumber y = MathCommands.GetNumber(aEnvironment, aStackTop, 2);
        int bin = aEnvironment.Precision();
        BigNumber z = new BigNumber(bin);
        z.Add(x,y,aEnvironment.Precision());
        RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
        return;
      }
    }
  }

  class LispSubtract extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      int length = LispStandard.InternalListLength(ARGUMENT(aEnvironment, aStackTop, 0));
      if (length == 2)
      {
        BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
        BigNumber z = new BigNumber(x);
        z.Negate(x);
        RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
        return;
      }
      else
      {
        BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
        BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
        BigNumber yneg = new BigNumber(y);
        yneg.Negate(y);
        BigNumber z = new BigNumber(aEnvironment.Precision());
        z.Add(x,yneg,aEnvironment.Precision());
        RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
        return;
      }
    }
  }

  class LispDivide extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      // if both arguments are integers, then BigNumber::Divide would perform an integer divide, but we want a float divide here.
      if (x.IsInt() && y.IsInt())
      {
        // why can't we just say BigNumber temp; ?
        BigNumber tempx = new BigNumber(aEnvironment.Precision());
        tempx.SetTo(x);
        tempx.BecomeFloat(aEnvironment.Precision());  // coerce x to float
        BigNumber tempy = new BigNumber(aEnvironment.Precision());
        tempy.SetTo(y);
        tempy.BecomeFloat(aEnvironment.Precision());  // coerce x to float
        z.Divide(tempx, tempy,aEnvironment.Precision());
      }
      else
      {
        z.Divide(x, y,aEnvironment.Precision());
      }
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
      return;
    }
  }

  class YacasBuiltinPrecisionSet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr index = new LispPtr();
      index.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get() != null, 1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get().String() != null, 1);

      int ind = Integer.parseInt(index.Get().String(),10);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ind>0,1);
      aEnvironment.SetPrecision(ind);
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispGetExactBits extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo(
        (x.IsInt())
      ? x.BitCount()  // for integers, return the bit count
        : LispStandard.digits_to_bits((long)(x.GetPrecision()), 10)   // for floats, return the precision
        );
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }
 

  class LispSetExactBits extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo(x);
 
    // do nothing for integers
    if (!(z.IsInt()))
      z.Precision((int)(LispStandard.bits_to_digits((long)(y.Double()), 10)));
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispBitCount extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo(x.BitCount());
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispMathSign extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo(x.Sign());
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispMathIsSmall extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop), x.IsSmall());
    }
  }

  class LispMathNegate extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.Negate(x);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispFloor extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.Floor(x);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispCeil extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.Negate(x);
      z.Floor(z);
      z.Negate(z);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispAbs extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo(x);
      if (x.Sign()<0)
        z.Negate(x);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispMod extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.Mod(x,y);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispDiv extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      if (x.IsInt() && y.IsInt())
      {  // both integer, perform integer division
          BigNumber z = new BigNumber(aEnvironment.Precision());
          z.Divide(x,y,aEnvironment.Precision());
          RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
          return;
      }
      else
      {
        throw new Exception("LispDiv: error: both arguments must be integer");
      }
    }
  }

  class LispBitsToDigits extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      long result = 0;  // initialize just in case
      if (x.IsInt() && x.IsSmall() && y.IsInt() && y.IsSmall())
      {
        // bits_to_digits uses unsigned long, see numbers.h
        int base = (int)y.Double();
        result = LispStandard.bits_to_digits((long)(x.Double()), base);
      }
      else
      {
        throw new Yacasexception("BitsToDigits: error: arguments ("+x.Double()+", "+y.Double()+") must be small integers");
      }
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo((long)result);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispDigitsToBits extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      long result = 0;  // initialize just in case
      if (x.IsInt() && x.IsSmall() && y.IsInt() && y.IsSmall())
      {
        // bits_to_digits uses unsigned long, see numbers.h
        int base = (int)y.Double();
        result = LispStandard.digits_to_bits((long)(x.Double()), base);
      }
      else
      {
        throw new Yacasexception("BitsToDigits: error: arguments ("+x.Double()+", "+y.Double()+") must be small integers");
      }
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo((long)result);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispGcd extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.Gcd(x,y);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispSystemCall extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
      String orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      String oper = LispStandard.InternalUnstringify(orig);
      String ls_str;
      Process ls_proc = Runtime.getRuntime().exec(oper);
      // get its output (your input) stream
      BufferedReader ls_in = new BufferedReader(new InputStreamReader(ls_proc.getInputStream()));

      while ((ls_str = ls_in.readLine()) != null)
      {
        aEnvironment.iCurrentOutput.Write(ls_str);
        aEnvironment.iCurrentOutput.Write("\n");
      }
    }
  }

  class LispFastArcSin extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x;
      x = GetNumber(aEnvironment, aStackTop, 1);
      double result = Math.asin(x.Double());
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo(result);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispFastLog extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x;
      x = GetNumber(aEnvironment, aStackTop, 1);
      double result = Math.log(x.Double());
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo(result);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispFastPower extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x, y;
      x = GetNumber(aEnvironment, aStackTop, 1);
      y = GetNumber(aEnvironment, aStackTop, 2);
      double result = Math.pow(x.Double(), y.Double());
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo(result);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispShiftLeft extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber n = GetNumber(aEnvironment, aStackTop, 2);
      long nrToShift = n.Long();
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.ShiftLeft(x,(int)nrToShift);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispShiftRight extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber n = GetNumber(aEnvironment, aStackTop, 2);
      long nrToShift = n.Long();
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.ShiftRight(x,(int)nrToShift);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispFromBase extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // Get the base to convert to:
      // Evaluate first argument, and store result in oper
      LispPtr oper = new LispPtr();
      oper.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      // Check that result is a number, and that it is in fact an integer
      BigNumber num = oper.Get().Number(aEnvironment.Precision());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,num != null,1);
    // check that the base is an integer between 2 and 32
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,num.IsInt(), 1);

      // Get a short platform integer from the first argument
      int base = (int)(num.Double());

      // Get the number to convert
      LispPtr fromNum = new LispPtr();
      fromNum.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
      String str2;
      str2 = fromNum.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str2 != null,2);

      // Added, unquote a string
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,LispStandard.InternalIsString(str2),2);
      str2 = aEnvironment.HashTable().LookUpUnStringify(str2);

      // convert using correct base
      BigNumber z = new BigNumber(str2,aEnvironment.Precision(),base);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispToBase extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // Get the base to convert to:
      // Evaluate first argument, and store result in oper
      LispPtr oper = new LispPtr();
      oper.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      // Check that result is a number, and that it is in fact an integer
      BigNumber num = oper.Get().Number(aEnvironment.Precision());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,num != null,1);
    // check that the base is an integer between 2 and 32
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,num.IsInt(), 1);

      // Get a short platform integer from the first argument
      int base = (int)(num.Long());

      // Get the number to convert
      BigNumber x = GetNumber(aEnvironment, aStackTop, 2);

      // convert using correct base
      String str;
      str = x.ToString(aEnvironment.Precision(),base);
      // Get unique string from hash table, and create an atom from it.

      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpStringify(str)));
    }
  }

  class LispMaxEvalDepth extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr index = new LispPtr();
      index.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get() != null, 1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get().String() != null, 1);

      int ind = Integer.parseInt(index.Get().String(),10);
      aEnvironment.iMaxEvalDepth = ind;
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispDefLoad extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_CORE(aEnvironment, aStackTop,aEnvironment.iSecure == false, LispError.KLispErrSecurityBreach);

      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      // Get file name
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

      LispStandard.LoadDefFile(aEnvironment, orig);
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispUse extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      // Get file name
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

      LispStandard.InternalUse(aEnvironment,orig);
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispRightAssociative extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // Get operator
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
      String orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      aEnvironment.iInfixOperators.SetRightAssociative(LispStandard.SymbolName(aEnvironment,orig));
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispLeftPrecedence extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // Get operator
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
      String orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

      LispPtr index = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, index, ARGUMENT(aEnvironment, aStackTop, 2));
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get() != null, 2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get().String() != null, 2);
      int ind = Integer.parseInt(index.Get().String(),10);

      aEnvironment.iInfixOperators.SetLeftPrecedence(LispStandard.SymbolName(aEnvironment,orig),ind);
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispRightPrecedence extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // Get operator
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
      String orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

      LispPtr index = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, index, ARGUMENT(aEnvironment, aStackTop, 2));
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get() != null, 2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get().String() != null, 2);
      int ind = Integer.parseInt(index.Get().String(),10);

      aEnvironment.iInfixOperators.SetRightPrecedence(LispStandard.SymbolName(aEnvironment,orig),ind);
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispIsBodied extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispInFixOperator op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iBodiedOperators);
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop), op != null);
    }
  }

  class LispIsInFix extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispInFixOperator op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iInfixOperators);
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop), op != null);
    }
  }

  class LispIsPreFix extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispInFixOperator op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iPrefixOperators);
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop), op != null);
    }
  }

  class LispIsPostFix extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispInFixOperator op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iPostfixOperators);
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop), op != null);
    }
  }

  class LispGetPrecedence extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispInFixOperator op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iInfixOperators);
      if (op == null)
      {  // also need to check for a postfix or prefix operator
        op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iPrefixOperators);
        if (op == null)
        {
          op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iPostfixOperators);
          if (op == null)
          {  // or maybe it's a bodied function
            op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iBodiedOperators);
            LispError.CHK_CORE(aEnvironment,aStackTop,op!=null, LispError.KLispErrIsNotInFix);
          }
        }
      }
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,""+op.iPrecedence));
    }
  }

  class LispGetLeftPrecedence extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispInFixOperator op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iInfixOperators);
      if (op == null)
      {  // infix and postfix operators have left precedence
        op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iPostfixOperators);
        LispError.CHK_CORE(aEnvironment,aStackTop,op!=null, LispError.KLispErrIsNotInFix);
      }
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,""+op.iLeftPrecedence));
    }
  }

  class LispGetRightPrecedence extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispInFixOperator op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iInfixOperators);
      if (op == null)
      {   // bodied, infix and prefix operators have right precedence
        op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iPrefixOperators);
        if (op == null)
        {   // or maybe it's a bodied function
          op = MathCommands.OperatorInfo(aEnvironment, aStackTop, aEnvironment.iBodiedOperators);
          LispError.CHK_CORE(aEnvironment,aStackTop,op!=null, LispError.KLispErrIsNotInFix);
        }
      }
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,""+op.iRightPrecedence));
    }
  }

  class YacasBuiltinPrecisionGet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // decimal precision
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,""+aEnvironment.Precision()));
    }
  }

  class LispBitAnd extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.BitAnd(x,y);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispBitOr extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.BitOr(x,y);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispBitXor extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      BigNumber y = GetNumber(aEnvironment, aStackTop, 2);
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.BitXor(x,y);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispSecure extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      boolean prevSecure = aEnvironment.iSecure;
      aEnvironment.iSecure = true;
      try
      {
        aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 1));
      }
      catch (Exception e) { throw e; }
      finally { aEnvironment.iSecure = prevSecure; }
    }
  }

  class LispFindFile extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_CORE(aEnvironment, aStackTop,aEnvironment.iSecure == false, LispError.KLispErrSecurityBreach);
 
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      // Get file name
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      String oper = LispStandard.InternalUnstringify(orig);

      String filename = LispStandard.InternalFindFile(oper, aEnvironment.iInputDirectories);
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpStringify(filename)));
    }
  }

  class LispFindFunction extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_CORE(aEnvironment, aStackTop,aEnvironment.iSecure == false, LispError.KLispErrSecurityBreach);
 
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      // Get file name
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
      String orig = evaluated.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      String oper = LispStandard.InternalUnstringify(orig);

      LispMultiUserFunction multiUserFunc =
          aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper));
      if (multiUserFunc != null)
      {
        LispDefFile def = multiUserFunc.iFileToOpen;
        if (def != null)
        {
          RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,def.iFileName));
          return;
        }
      }
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,"\"\""));
    }
  }

  class LispIsGeneric extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop), evaluated.Get().Generic() != null);
    }
  }

  class LispGenericTypeName extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get().Generic() != null,1);
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,evaluated.Get().Generic().TypeName()));
    }
  }

  class GenArrayCreate extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr sizearg = new LispPtr();
      sizearg.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get() != null, 1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get().String() != null, 1);

      int size = Integer.parseInt(sizearg.Get().String(),10);

      LispPtr initarg = new LispPtr();
      initarg.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
 
      ArrayClass array = new ArrayClass(size,initarg.Get());
      RESULT(aEnvironment, aStackTop).Set(LispGenericClass.New(array));
    }
  }

  class GenArraySize extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      GenericClass gen = evaluated.Get().Generic();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,gen != null,1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,gen.TypeName().equals("\"Array\""),1);
      int size=((ArrayClass)gen).Size();
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,""+size));
    }
  }

  class GenArrayGet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      GenericClass gen = evaluated.Get().Generic();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,gen != null,1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,gen.TypeName().equals("\"Array\""),1);

      LispPtr sizearg = new LispPtr();
      sizearg.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());

      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get() != null, 2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get().String() != null, 2);

      int size = Integer.parseInt(sizearg.Get().String(),10);

      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,size>0 && size<=((ArrayClass)gen).Size(),2);
      LispObject object = ((ArrayClass)gen).GetElement(size);

      RESULT(aEnvironment, aStackTop).Set(object.Copy(false));
    }
  }

  class GenArraySet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      GenericClass gen = evaluated.Get().Generic();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,gen != null,1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,gen.TypeName().equals("\"Array\""),1);

      LispPtr sizearg = new LispPtr();
      sizearg.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());

      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get() != null, 2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get().String() != null, 2);

      int size = Integer.parseInt(sizearg.Get().String(),10);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,size>0 && size<=((ArrayClass)gen).Size(),2);

      LispPtr obj = new LispPtr();
      obj.Set(ARGUMENT(aEnvironment, aStackTop, 3).Get());
      ((ArrayClass)gen).SetElement(size,obj.Get());
      LispStandard.InternalTrue( aEnvironment, RESULT(aEnvironment, aStackTop));
    }
  }

  class LispCustomEval extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : CustomEval");////TODO fixme
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispCustomEvalExpression extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : CustomEvalExpression");////TODO fixme
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispCustomEvalResult extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : CustomEvalResult");////TODO fixme
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispCustomEvalLocals extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : LispCustomEvalLocals");////TODO fixme
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispCustomEvalStop extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : LispCustomEvalStop");////TODO fixme
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispTraceRule extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : LispTraceRule");////TODO fixme
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispTraceStack extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : TraceStack");////TODO fixme
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispReadLisp extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispParser parser = new LispParser(aEnvironment.iCurrentTokenizer,
                        aEnvironment.iCurrentInput,
                        aEnvironment);
      // Read expression
      parser.Parse(RESULT(aEnvironment, aStackTop));
    }
  }

  class LispReadLispListed extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispParser parser = new LispParser(aEnvironment.iCurrentTokenizer,
                        aEnvironment.iCurrentInput,
                        aEnvironment);
      parser.iListed = true;
      // Read expression
      parser.Parse(RESULT(aEnvironment, aStackTop));
    }
  }

  class LispType extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispPtr subList = evaluated.Get().SubList();
      LispObject head = null;
      if (subList == null)
      {
        RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,"\"\""));
        return;
      }
      head = subList.Get();
      if (head.String() == null)
      {
        RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,"\"\""));
        return;
      }
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpStringify(head.String())));
      return;
    }
  }

  class YacasStringMidGet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 3).Get());
      LispError.CHK_ISSTRING_CORE(aEnvironment,aStackTop,evaluated,3);
      String orig = evaluated.Get().String();
 
      LispPtr index = new LispPtr();
      index.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get() != null, 1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get().String() != null, 1);
      int from = Integer.parseInt(index.Get().String(),10);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,from>0,1);
 
      index.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get() != null, 2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get().String() != null, 2);
      int count = Integer.parseInt(index.Get().String(),10);

 
      String str = "\""+orig.substring(from,from+count)+"\"";
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,str));
    }
  }

  class YacasStringMidSet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr evaluated = new LispPtr();
      evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 3).Get());
      LispError.CHK_ISSTRING_CORE(aEnvironment,aStackTop,evaluated,3);
      String orig = evaluated.Get().String();
      LispPtr index = new LispPtr();
      index.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get() != null, 1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,index.Get().String() != null, 1);
      int from = Integer.parseInt(index.Get().String(),10);

      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,from>0,1);
 
      LispPtr ev2 = new LispPtr();
      ev2.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
      LispError.CHK_ISSTRING_CORE(aEnvironment,aStackTop,ev2,2);
      String replace = ev2.Get().String();

      LispError.CHK_CORE(aEnvironment, aStackTop,from+replace.length()-2<orig.length(), LispError.KLispErrInvalidArg);
      String str;
      str = orig.substring(0,from);
      str = str + replace.substring(1,replace.length()-1);
//System.out.println("from="+from+replace.length()-2);
      str = str + orig.substring(from+replace.length()-2,orig.length());
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,str));
    }
  }

  class GenPatternCreate extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr pattern = new LispPtr();
      pattern.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispPtr postpredicate = new LispPtr();
      postpredicate.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());

      LispIterator iter = new LispIterator(pattern);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,iter.GetObject() != null,1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,iter.GetObject().SubList() != null,1);
      iter.GoSub();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,iter.GetObject() != null,1);
      iter.GoNext();

      LispPtr ptr = iter.Ptr();


      YacasPatternPredicateBase matcher =
          new YacasPatternPredicateBase(aEnvironment, ptr,postpredicate);
      PatternClass p = new PatternClass(matcher);
      RESULT(aEnvironment, aStackTop).Set(LispGenericClass.New(p));
    }
  }

  class GenPatternMatches extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr pattern = new LispPtr();
      pattern.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      GenericClass gen = pattern.Get().Generic();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,gen != null,1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,gen.TypeName().equals("\"Pattern\""),1);

      LispPtr list = new LispPtr();
      list.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());

      PatternClass patclass = (PatternClass)gen;

      LispIterator iter = new LispIterator(list);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,iter.GetObject() != null,2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,iter.GetObject().SubList() != null,2);
      iter.GoSub();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,iter.GetObject() != null,2);
      iter.GoNext();

      LispPtr ptr = iter.Ptr();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ptr != null,2);
      boolean matches = patclass.Matches(aEnvironment,ptr);
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop),matches);
    }
  }

  class LispRuleBaseDefined extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr name = new LispPtr();
      name.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      String orig = name.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      String oper = LispStandard.InternalUnstringify(orig);

      LispPtr sizearg = new LispPtr();
      sizearg.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get() != null, 2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get().String() != null, 2);

      int arity = Integer.parseInt(sizearg.Get().String(),10);

      LispUserFunction userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper),arity);
      LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop),userFunc != null);
    }
  }

  class LispDefLoadFunction extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr name = new LispPtr();
      name.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      String orig = name.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      String oper = LispStandard.InternalUnstringify(orig);

      LispMultiUserFunction multiUserFunc =
          aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper));
      if (multiUserFunc != null)
      {
        if (multiUserFunc.iFileToOpen!=null)
        {
          LispDefFile def = multiUserFunc.iFileToOpen;
          if (!def.iIsLoaded)
          {
            multiUserFunc.iFileToOpen=null;
            LispStandard.InternalUse(aEnvironment,def.iFileName);
          }
        }
      }
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispRuleBaseArgList extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr name = new LispPtr();
      name.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      String orig = name.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
      String oper = LispStandard.InternalUnstringify(orig);

      LispPtr sizearg = new LispPtr();
      sizearg.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get() != null, 2);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get().String() != null, 2);

      int arity = Integer.parseInt(sizearg.Get().String(),10);

      LispUserFunction userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper),arity);
      LispError.CHK_CORE(aEnvironment, aStackTop,userFunc != null, LispError.KLispErrInvalidArg);

      LispPtr list = userFunc.ArgList();
      LispPtr head = new LispPtr();
      head.Set(aEnvironment.iList.Copy(false));
      head.Get().Next().Set(list.Get());
      RESULT(aEnvironment, aStackTop).Set(LispSubList.New(head.Get()));
    }
  }

  class LispNewRulePattern extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InternalNewRulePattern(aEnvironment, aStackTop, false);
    }
  }

  class LispMacroNewRulePattern extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      InternalNewRulePattern(aEnvironment, aStackTop, true
      );
    }
  }

  class LispSubst extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr from = new LispPtr(),to = new LispPtr(),body = new LispPtr();
      from.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      to  .Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
      body.Set(ARGUMENT(aEnvironment, aStackTop, 3).Get());
      SubstBehaviour behaviour = new SubstBehaviour(aEnvironment,from, to);
      LispStandard.InternalSubstitute(RESULT(aEnvironment, aStackTop), body, behaviour);
    }
  }

  class LispLocalSymbols extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      int nrArguments = LispStandard.InternalListLength(ARGUMENT(aEnvironment, aStackTop, 0));
      int nrSymbols = nrArguments-2;

      String names[] = new String[nrSymbols];
      String localnames[] = new String[nrSymbols];

      int uniquenumber = aEnvironment.GetUniqueId();
      int i;
      for (i=0;i<nrSymbols;i++)
      {
        String atomname = Argument(ARGUMENT(aEnvironment, aStackTop, 0), i+1).Get().String();
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,atomname != null, i+1);
        names[i] = atomname;
        int len = atomname.length();
        String newname = "$"+atomname+uniquenumber;
        String variable = aEnvironment.HashTable().LookUp(newname);
        localnames[i] = variable;
      }
      LocalSymbolBehaviour behaviour = new LocalSymbolBehaviour(aEnvironment,names,localnames,nrSymbols);
      LispPtr result = new LispPtr();
      LispStandard.InternalSubstitute(result, Argument(ARGUMENT(aEnvironment, aStackTop, 0), nrArguments-1), behaviour);
      aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), result);
    }
  }

  class LispFastIsPrime extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      //TODO fixme this routine should actually be called SlowIsPrime ;-)
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      long n = x.Long();
      long result = 1;

      // We only want people to pass in small integers
      if (n>65538)
        result = 0;
 
      int i=2;
      int max = (int)(1+Math.sqrt(n));
//System.out.println("n = "+n+" max = "+max);
      while (i<=max && result == 1)
      {
      //System.out.println(""+n+"%"+i+" = "+(n%i));
        if ((n%i) == 0)
          result = 0;
        i++;
      }
 
      BigNumber z = new BigNumber(aEnvironment.Precision());
      z.SetTo(result);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
    }
  }

  class LispFac extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get().Number(0) != null,1);
      LispPtr arg = ARGUMENT(aEnvironment, aStackTop, 1);
 
      //TODO fixme I am sure this can be optimized still
      int nr = (int)arg.Get().Number(0).Long();
      LispError.Check(nr>=0,LispError.KLispErrInvalidArg);
      BigNumber fac = new BigNumber("1",10,10);
      int i;
      for (i=2;i<=nr;i++)
      {
        BigNumber m = new BigNumber(""+i,10,10);
        m.Multiply(fac,m,0);
        fac = m;
      }
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(fac));
    }
  }

  class LispApplyPure extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr oper = new LispPtr();
      oper.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispPtr args = new LispPtr();
      args.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());

      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,args.Get().SubList() != null,2);
      LispError.CHK_CORE(aEnvironment, aStackTop,args.Get().SubList().Get() != null,2);

      // Apply a pure string
      if (oper.Get().String() != null)
      {
        LispStandard.InternalApplyString(aEnvironment, RESULT(aEnvironment, aStackTop),
                    oper.Get().String(),
                    args.Get().SubList().Get().Next());
      }
      else
      {   // Apply a pure function {args,body}.
        LispPtr args2 = new LispPtr();
        args2.Set(args.Get().SubList().Get().Next().Get());
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,oper.Get().SubList() != null,1);
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,oper.Get().SubList().Get() != null,1);
        LispStandard.InternalApplyPure(oper,args2,RESULT(aEnvironment, aStackTop),aEnvironment);
      }
    }
  }


  class YacasPrettyReaderSet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      int nrArguments = LispStandard.InternalListLength(ARGUMENT(aEnvironment, aStackTop, 0));
      if (nrArguments == 1)
      {
        aEnvironment.iPrettyReader = null;
      }
      else
      {
        LispError.CHK_CORE(aEnvironment, aStackTop,nrArguments == 2,LispError.KLispErrWrongNumberOfArgs);
        LispPtr oper = new LispPtr();
        oper.Set(ARGUMENT(aEnvironment, aStackTop, 0).Get());
        oper.GoNext();
        LispError.CHK_ISSTRING_CORE(aEnvironment,aStackTop,oper,1);
        aEnvironment.iPrettyReader = oper.Get().String();
      }
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class YacasPrettyReaderGet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      if (aEnvironment.iPrettyReader == null)
        RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,"\"\""));
      else
        RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.iPrettyReader));
    }
  }

  class YacasPrettyPrinterSet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      int nrArguments = LispStandard.InternalListLength(ARGUMENT(aEnvironment, aStackTop, 0));
      if (nrArguments == 1)
      {
        aEnvironment.iPrettyPrinter = null;
      }
      else
      {
        LispError.CHK_CORE(aEnvironment, aStackTop,nrArguments == 2,LispError.KLispErrWrongNumberOfArgs);
        LispPtr oper = new LispPtr();
        oper.Set(ARGUMENT(aEnvironment, aStackTop, 0).Get());
        oper.GoNext();
        LispError.CHK_ISSTRING_CORE(aEnvironment,aStackTop,oper,1);
        aEnvironment.iPrettyPrinter = oper.Get().String();
      }
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class YacasPrettyPrinterGet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      if (aEnvironment.iPrettyPrinter == null)
        RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,"\"\""));
      else
        RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.iPrettyPrinter));
    }
  }

  class LispGarbageCollect extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.HashTable().GarbageCollect();
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispPatchLoad extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : PatchLoad");//TODO FIXME
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispPatchString extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : PatchString");//TODO FIXME
      throw new Yacasexception("Function not yet supported");
    }
  }

  class YacasExtraInfoSet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr object = new LispPtr();
      object.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      LispPtr info = new LispPtr();
      info.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());

      RESULT(aEnvironment, aStackTop).Set( object.Get().SetExtraInfo(info) );
    }
  }

  class YacasExtraInfoGet extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr object = new LispPtr();
      object.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

      LispPtr result = object.Get().ExtraInfo();
      if (result == null)
      {
        LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));
      }
      else if (result.Get() == null)
      {
        LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));
      }
      else
      {
        RESULT(aEnvironment, aStackTop).Set(result.Get());
      }
    }
  }

  class LispDefaultTokenizer extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentTokenizer = aEnvironment.iDefaultTokenizer;
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispCommonLispTokenizer extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : LispCommonLispTokenizer");//TODO FIXME
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispXmlTokenizer extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentTokenizer = aEnvironment.iXmlTokenizer;
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispExplodeTag extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr out = new LispPtr();
      out.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
      LispError.CHK_ISSTRING_CORE(aEnvironment,aStackTop,out,1);

      String str = out.Get().String();
      int strInd = 0;
      strInd++;
      if (str.charAt(strInd) != '<')
      {
        RESULT(aEnvironment, aStackTop).Set(out.Get());
        return;
      }
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str.charAt(strInd) == '<',1);
      strInd++;
      String type = "\"Open\"";

      if (str.charAt(strInd) == '/')
      {
        type = "\"Close\"";
        strInd++;
      }
      String tag = new String();
 
      tag = tag + "\"";
      while (LispTokenizer.IsAlpha(str.charAt(strInd)))
      {
        char c = str.charAt(strInd);
        strInd++;
        if (c >= 'a' && c <= 'z')
            c = (char)(c + ('A'-'a'));
        tag = tag + c;
      }
      tag = tag + "\"";

      LispObject info = null;

      while (str.charAt(strInd) == ' ') strInd++;
      while (str.charAt(strInd) != '>' && str.charAt(strInd) != '/')
      {
        String name = new String();
        name = name + "\"";

        while (LispTokenizer.IsAlpha(str.charAt(strInd)))
        {
            char c = str.charAt(strInd);
            strInd++;
            if (c >= 'a' && c <= 'z')
                c = (char)(c + ('A'-'a'));
            name = name + c;
        }
        name = name + "\"";
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str.charAt(strInd) == '=',1);
        strInd++;
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str.charAt(strInd) == '\"',1);
        String value = new String();

        value = value + (str.charAt(strInd));
        strInd++;
        while (str.charAt(strInd) != '\"')
        {
            value = value + (str.charAt(strInd));
            strInd++;
        }
        value = value + (str.charAt(strInd));
        strInd++;

//printf("[%s], [%s]\n",name.String(),value.String());
        {
          LispObject ls = LispAtom.New(aEnvironment,"List");
          LispObject nm = LispAtom.New(aEnvironment,name);
          LispObject vl = LispAtom.New(aEnvironment,value);
          nm.Next().Set(vl);
          ls.Next().Set(nm);
          LispObject newinfo =  LispSubList.New(ls);
          newinfo.Next().Set(info);
          info = newinfo;
        }
        while (str.charAt(strInd) == ' ') strInd++;

//printf("End is %c\n",str[0]);
    }
    if (str.charAt(strInd) == '/')
    {
      type = "\"OpenClose\"";
      strInd++;
      while (str.charAt(strInd) == ' ') strInd++;
    }
 
    {
      LispObject ls = LispAtom.New(aEnvironment,"List");
      ls.Next().Set(info);
      info = LispSubList.New(ls);
    }

    LispObject xm = LispAtom.New(aEnvironment,"XmlTag");
    LispObject tg = LispAtom.New(aEnvironment,tag);
    LispObject tp = LispAtom.New(aEnvironment,type);
    info.Next().Set(tp);
    tg.Next().Set(info);
    xm.Next().Set(tg);
    RESULT(aEnvironment, aStackTop).Set(LispSubList.New(xm));

    }
  }

  class YacasBuiltinAssoc extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      // key to find
      LispPtr key = new LispPtr();
      key.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
 
      // assoc-list to find it in
      LispPtr list = new LispPtr();
      list.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());

      LispObject t;

      //Check that it is a compound object
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,list.Get().SubList() != null, 2);
      t = list.Get().SubList().Get();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,t != null, 2);
      t = t.Next().Get();

      while (t != null)
      {
        if (t.SubList() != null)
        {
          LispObject sub = t.SubList().Get();
          if (sub != null)
          {
            sub = sub.Next().Get();
            LispPtr temp = new LispPtr();
            temp.Set(sub);
            if(LispStandard.InternalEquals(aEnvironment,key,temp))
            {
              RESULT(aEnvironment, aStackTop).Set(t);
              return;
            }
          }
        }
        t = t.Next().Get();
      }
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,"Empty"));
    }
  }

  class LispCurrentFile extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpStringify(aEnvironment.iInputStatus.FileName())));
    }
  }

  class LispCurrentLine extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,""+aEnvironment.iInputStatus.LineNumber()));
    }
  }

  class LispBackQuote extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BackQuoteBehaviour behaviour = new BackQuoteBehaviour(aEnvironment);
      LispPtr result = new LispPtr();
      LispStandard.InternalSubstitute(result, ARGUMENT(aEnvironment, aStackTop,  1), behaviour);
      aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), result);
    }
  }

  class LispDumpBigNumberDebugInfo extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      BigNumber x = GetNumber(aEnvironment, aStackTop, 1);
      x.DumpDebugInfo(aEnvironment.iCurrentOutput);
      LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispInDebugMode extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispDebugFile extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      throw new Exception("Cannot call DebugFile in non-debug version of Yacas");
    }
  }

  class LispDebugLine extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      throw new Exception("Cannot call DebugLine in non-debug version of Yacas");
    }
  }

  class LispVersion extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      RESULT(aEnvironment,aStackTop).Set(LispAtom.New(aEnvironment,"\""+CVersion.VERSION+"\""));
    }
  }


  class LispExit extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      Runtime.getRuntime().exit(0);
    }
  }

  class LispExitRequested extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));
    }
  }

  class LispHistorySize extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : LispHistorySize");//TODO FIXME
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispStackSize extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : LispStackSize");//TODO FIXME
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispIsPromptShown extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : LispIsPromptShown");//TODO FIXME
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispReadCmdLineString extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      aEnvironment.iCurrentOutput.Write("Function not yet implemented : LispReadCmdLineString");//TODO FIXME
      throw new Yacasexception("Function not yet supported");
    }
  }

  class LispTime extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      long starttime = System.currentTimeMillis();
      LispPtr res = new LispPtr();
      aEnvironment.iEvaluator.Eval(aEnvironment, res, ARGUMENT(aEnvironment,aStackTop,1));
      long endtime = System.currentTimeMillis();
      double timeDiff;
      timeDiff = endtime-starttime;
      timeDiff /= 1000.0;
      RESULT(aEnvironment,aStackTop).Set(LispAtom.New(aEnvironment,""+timeDiff));
    }
  }

  class LispFileSize extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception
    {
      LispPtr fnameObject = new LispPtr();
      fnameObject.Set(ARGUMENT(aEnvironment,aStackTop,1).Get());
      LispError.CHK_ISSTRING_CORE(aEnvironment,aStackTop,fnameObject,1);
      String fname = LispStandard.InternalUnstringify(fnameObject.Get().String());
      String hashedname = aEnvironment.HashTable().LookUp(fname);

      long fileSize = 0;
      InputStatus oldstatus = new InputStatus(aEnvironment.iInputStatus);
      aEnvironment.iInputStatus.SetTo(hashedname);
      try
      {
        // Open file
        LispInput newInput = // new StdFileInput(hashedname, aEnvironment.iInputStatus);
            LispStandard.OpenInputFile(aEnvironment, aEnvironment.iInputDirectories, hashedname, aEnvironment.iInputStatus);
 
        LispError.Check(newInput != null, LispError.KLispErrFileNotFound);
        fileSize = newInput.StartPtr().length();
      }
      catch (Exception e)
      {
        throw e;
      }
      finally
      {
        aEnvironment.iInputStatus.RestoreFrom(oldstatus);
      }
      RESULT(aEnvironment,aStackTop).Set(LispAtom.New(aEnvironment,""+fileSize));
    }
  }


}

