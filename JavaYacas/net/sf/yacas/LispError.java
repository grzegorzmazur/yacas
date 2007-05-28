package net.sf.yacas;


class LispError
{
  static String ErrorString(int aError) throws Exception
  {
    LISPASSERT(aError>=0 && aError < KLispNrErrors);
//    switch (aError)
    {
      if (aError ==  KLispErrNone)
          return "No error";
      if (aError ==  KLispErrInvalidArg)
          return "Invalid argument";
      if (aError ==  KLispErrWrongNumberOfArgs)
          return "Wrong number of arguments";
      if (aError ==  KLispErrNotList)
          return "Argument is not a list";
      if (aError ==  KLispErrListNotLongEnough)
          return "List not long enough";
      if (aError ==  KLispErrInvalidStack)
          return "Invalid stack";
      if (aError ==  KQuitting)
          return "Quitting...";
      if (aError ==  KLispErrNotEnoughMemory)
          return "Not enough memory";
      if (aError ==  KInvalidToken)
          return "Empty token during parsing";
      if (aError ==  KLispErrInvalidExpression)
          return "Error parsing expression";
      if (aError ==  KLispErrUnprintableToken)
          return "Unprintable atom";
      if (aError ==  KLispErrFileNotFound)
          return "File not found";
      if (aError ==  KLispErrReadingFile)
          return "Error reading file";
      if (aError ==  KLispErrCreatingUserFunction)
          return "Could not create user function";
      if (aError ==  KLispErrCreatingRule)
          return "Could not create rule";
      if (aError ==  KLispErrArityAlreadyDefined)
          return "Rule base with this arity already defined";
      if (aError ==  KLispErrCommentToEndOfFile)
          return "Reaching end of file within a comment block";
      if (aError ==  KLispErrNotString)
          return "Argument is not a string";
      if (aError ==  KLispErrNotInteger)
          return "Argument is not an integer";
      if (aError ==  KLispErrParsingInput)
          return "Error while parsing input";
      if (aError ==  KLispErrMaxRecurseDepthReached)
          return "Max evaluation stack depth reached.\nPlease use MaxEvalDepth to increase the stack size as needed.";
      if (aError ==  KLispErrDefFileAlreadyChosen)
          return "DefFile already chosen for function";
      if (aError ==  KLispErrDivideByZero)
          return "Divide by zero";
      if (aError ==  KLispErrNotAnInFixOperator)
          return "Trying to make a non-infix operator right-associative";
      if (aError ==  KLispErrIsNotInFix)
          return "Trying to get precedence of non-infix operator";
      if (aError ==  KLispErrSecurityBreach)
          return "Trying to perform an insecure action";
      if (aError ==  KLispErrLibraryNotFound)
          return "Could not find library";
      if (aError ==  KLispErrUserInterrupt)
          return "User interrupted calculation";
      if (aError ==  KLispErrNonBooleanPredicateInPattern)
          return "Predicate doesn't evaluate to a boolean in pattern";
     if (aError ==  KLispErrGenericFormat) return "Generic format";
    }
    return "Unspecified Error";
  }
  static void Check(boolean hastobetrue, int aError) throws Exception
  {
    if (!hastobetrue)
    {
      String error = ErrorString(aError);//"Error number "+aError+" (//TODO FIXME still need to port over the string table)";
      throw new Yacasexception(error);
    }
  }
  static void RaiseError(String str) throws Exception
  {
      throw new Yacasexception(str);
  }

  public static void CheckNrArgs(int n, LispPtr aArguments, LispEnvironment aEnvironment) throws Exception
  {
    int nrArguments = LispStandard.InternalListLength(aArguments);
    if (nrArguments != n)
    {
      ErrorNrArgs(n-1, nrArguments-1, aArguments, aEnvironment);
    }
  }
  static void ErrorNrArgs(int needed, int passed, LispPtr aArguments, LispEnvironment aEnvironment) throws Exception
  {
    if (aArguments.Get() == null)
    {
      throw new Yacasexception("Error in compiled code.");
    }
    else
    {
//TODO FIXME      ShowStack(aEnvironment);
      String error = ShowFunctionError(aArguments, aEnvironment) + "expected "+needed+" arguments, got "+passed;
      throw new Yacasexception(error);

/*TODO FIXME
      LispChar str[20];
      aEnvironment.iErrorOutput.Write("expected ");
      InternalIntToAscii(str,needed);
      aEnvironment.iErrorOutput.Write(str);
      aEnvironment.iErrorOutput.Write(" arguments, got ");
      InternalIntToAscii(str,passed);
      aEnvironment.iErrorOutput.Write(str);
      aEnvironment.iErrorOutput.Write("\n");
      LispError.Check(passed == needed,LispError.KLispErrWrongNumberOfArgs);
*/
    }
  }

  static String ShowFunctionError(LispPtr aArguments, LispEnvironment aEnvironment) throws Exception
  {
    if (aArguments.Get() == null)
    {
      return "Error in compiled code. ";
    }
    else
    {
      String string = aArguments.Get().String();
      if (string != null)
      {
        return "In function \"" + string + "\" : ";
      }
    }
    return "[Atom]";
  }
  public static void CHK_CORE(LispEnvironment aEnvironment,int aStackTop,boolean aPredicate ,int errNo) throws Exception
  {
    if (!aPredicate)
    {
        LispPtr arguments = YacasEvalCaller.ARGUMENT(aEnvironment,aStackTop,0);
        if (arguments.Get() == null)
        {
          throw new Yacasexception("Error in compiled code\n");
        }
        else
        {
          String error = "";
//TODO FIXME          ShowStack(aEnvironment);
          error = error + ShowFunctionError(arguments, aEnvironment) + "generic error";
          throw new Yacasexception(error);
        }
      }
  }
  public static void LISPASSERT(boolean aPredicate) throws Exception
  {
    if (!aPredicate)
      throw new Yacasexception("Assertion failed");
  }
  public static void CHK_ARG_CORE(LispEnvironment aEnvironment,int aStackTop,boolean aPredicate,int aArgNr) throws Exception
  {
    CheckArgTypeWithError(aEnvironment, aStackTop, aPredicate, aArgNr,"");
  }
  public static void CHK_ISLIST_CORE(LispEnvironment aEnvironment,int aStackTop,LispPtr evaluated,int aArgNr) throws Exception
  {
    CheckArgTypeWithError(aEnvironment, aStackTop, LispStandard.InternalIsList(evaluated), aArgNr,"argument is not a list");
  }
  public static void CHK_ISSTRING_CORE(LispEnvironment aEnvironment,int aStackTop,LispPtr evaluated,int aArgNr) throws Exception
  {
    CheckArgTypeWithError(aEnvironment, aStackTop, LispStandard.InternalIsString(evaluated.Get().String()), aArgNr,"argument is not a string");
  }
  public static void CheckArgTypeWithError(LispEnvironment aEnvironment,int aStackTop,boolean aPredicate,int aArgNr, String aErrorDescription) throws Exception
  {
    if (!aPredicate)
    {
        LispPtr arguments = YacasEvalCaller.ARGUMENT(aEnvironment,aStackTop,0);
        if (arguments.Get() == null)
        {
          throw new Yacasexception("Error in compiled code\n");
        }
        else
        {
          String error = "";
//TODO FIXME          ShowStack(aEnvironment);
          error = error + ShowFunctionError(arguments, aEnvironment) + "\nbad argument number "+aArgNr+"(counting from 1) : \n"+aErrorDescription + "\n";
          LispPtr arg = YacasEvalCaller.Argument(arguments,aArgNr);
          String strout;
 
          error = error + "The offending argument ";
          strout = LispStandard.PrintExpression(arg, aEnvironment, 60);
          error = error + strout;

          LispPtr eval = new LispPtr();
          aEnvironment.iEvaluator.Eval(aEnvironment, eval, arg);
          error = error + " evaluated to ";
          strout = LispStandard.PrintExpression(eval, aEnvironment, 60);
          error = error + strout;
          error = error + "\n";

          throw new Yacasexception(error);
        }
    }
  }


  static int KLispErrNone                   = 0;
  static int KLispErrInvalidArg             = 1;
  static int KLispErrWrongNumberOfArgs      = 2;
  static int KLispErrNotList                = 3;
  static int KLispErrListNotLongEnough      = 4;
  static int KLispErrInvalidStack           = 5;
  static int KQuitting                      = 6;
  static int KLispErrNotEnoughMemory        = 7;
  static int KInvalidToken                  = 8;
  static int KLispErrInvalidExpression      = 9;
  static int KLispErrUnprintableToken       = 10;
  static int KLispErrFileNotFound           = 11;
  static int KLispErrReadingFile            = 12;
  static int KLispErrCreatingUserFunction   = 13;
  static int KLispErrCreatingRule           = 14;
  static int KLispErrArityAlreadyDefined    = 15;
  static int KLispErrCommentToEndOfFile     = 16;
  static int KLispErrNotString              = 17;
  static int KLispErrNotInteger             = 18;
  static int KLispErrParsingInput           = 19;
  static int KLispErrMaxRecurseDepthReached = 20;
  static int KLispErrDefFileAlreadyChosen   = 21;
  static int KLispErrDivideByZero           = 22;
  static int KLispErrNotAnInFixOperator     = 23;
  static int KLispErrIsNotInFix             = 24;
  static int KLispErrSecurityBreach         = 25;
  static int KLispErrLibraryNotFound        = 26;
  static int KLispErrUserInterrupt          = 27;
  static int KLispErrNonBooleanPredicateInPattern = 28;
  static int KLispErrGenericFormat          = 29;
  static int KLispNrErrors                  = 30;


}