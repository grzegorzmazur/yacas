
#include "yacasprivate.h"
#include "lispenvironment.h"
#include "standard.h"
#include "lispeval.h"
#include "errors.h"
#include "infixparser.h"

#ifdef HAVE_STDIO_H
  #include <stdio.h> // Safe, only included if HAVE_STDIO_H defined
#endif

#ifdef HAVE_STDARG_H
  #include <stdarg.h>
#endif

#define InternalEval aEnvironment.iEvaluator->Eval


void ShowStack(LispEnvironment& aEnvironment)
{
    aEnvironment.iEvaluator->ShowStack(aEnvironment,
                                       aEnvironment.iErrorOutput);
}



void ShowFunctionError(LispPtr& aArguments,
                       LispEnvironment& aEnvironment)
{
  if (!aArguments)
  {
    aEnvironment.iErrorOutput.Write("Error in compiled code\n");
  }
  else
  {
    LispString * string = aArguments->String();
    if (string)
    {
      aEnvironment.iErrorOutput.Write("In function \"");
      aEnvironment.iErrorOutput.Write(string->c_str());
      aEnvironment.iErrorOutput.Write("\" : \n");
    }
  }
}


void CheckNrArgs(LispInt n, LispPtr& aArguments,
                 LispEnvironment& aEnvironment)
{
    LispInt nrArguments = InternalListLength(aArguments);
    if (nrArguments == n) return;
 
  LispInt needed = n-1;
  LispInt passed = nrArguments-1;

  if (!aArguments)
    {
      aEnvironment.iErrorOutput.Write("Error in compiled code\n");
    }
    else
    {
      ShowStack(aEnvironment);
      ShowFunctionError(aArguments, aEnvironment);

      LispChar str[20];
      aEnvironment.iErrorOutput.Write("expected ");
      InternalIntToAscii(str,needed);
      aEnvironment.iErrorOutput.Write(str);
      aEnvironment.iErrorOutput.Write(" arguments, got ");
      InternalIntToAscii(str,passed);
      aEnvironment.iErrorOutput.Write(str);
      aEnvironment.iErrorOutput.Write("\n");
      Check(passed == needed,KLispErrWrongNumberOfArgs);
    }
}


void CheckFuncGeneric(LispInt aError,LispPtr& aArguments,
                      LispEnvironment& aEnvironment)
{
    if (!aArguments)
    {
      aEnvironment.iErrorOutput.Write("Error in compiled code\n");
    }
    else
    {
      ShowStack(aEnvironment);
      ShowFunctionError(aArguments, aEnvironment);
      Check(0,aError);
    }
}

void CheckFuncGeneric(LispInt aError,
                      LispEnvironment& aEnvironment)
{
    ShowStack(aEnvironment);
    Check(0,aError);
}

void CheckArgType(LispInt aArgNr, LispPtr& aArguments,LispEnvironment& aEnvironment,
                  LispInt aError)
{
  if (!aArguments)
    {
        aEnvironment.iErrorOutput.Write("Error in compiled code\n");
    }
    else
    {
        ShowStack(aEnvironment);
        ShowFunctionError(aArguments, aEnvironment);
 
        aEnvironment.iErrorOutput.Write("bad argument number ");
        LispChar str[20];
        InternalIntToAscii(str,aArgNr);
        aEnvironment.iErrorOutput.Write(str);
        aEnvironment.iErrorOutput.Write(" (counting from 1)\n");
 
#define LIM_AL 60
        LispPtr& arg = Argument(aArguments,aArgNr);
        LispString strout;
 
        aEnvironment.iErrorOutput.Write("The offending argument ");
        PrintExpression(strout, arg, aEnvironment, LIM_AL);
        aEnvironment.iErrorOutput.Write(strout.c_str());
  //        aEnvironment.iErrorOutput.Write("\n");
 
  //        aEnvironment.iErrorOutput.Write("Argument ");
  //        PrintExpression(strout, arg, aEnvironment, LIM_AL);
  //        aEnvironment.iErrorOutput.Write(strout.String());
 
        LispPtr eval;
        InternalEval(aEnvironment, eval, arg);
        aEnvironment.iErrorOutput.Write(" evaluated to ");
        PrintExpression(strout, eval, aEnvironment, LIM_AL);
        aEnvironment.iErrorOutput.Write(strout.c_str());
        aEnvironment.iErrorOutput.Write("\n");

        DBG_( printf("Problem occurred at %s(%d)\n",
      aArguments->iFileName,
      aArguments->iLine ); )
    }
    Check(0,aError);
}

char *GenericErrorBuf()
{
   // PLEASECHECK TODO This is a global!
   static char theGenericErrorBuf[512];
   return theGenericErrorBuf;
}

void RaiseError(char* str,...)
{
#ifdef HAVE_STDARG_H
  va_list arg;
  va_start (arg, str);
 #ifdef HAVE_VSNPRINTF
  vsnprintf (GenericErrorBuf(), 500, str, arg);
 #else
  /* Just cross fingers and hope the buffer is large enough */
  vsprintf (GenericErrorBuf(), str, arg);
 #endif
  va_end (arg);
#else
  PlatMemCopy(GenericErrorBuf(), str, PlatStrLen(str));
#endif
  Check(LispFalse,KLispErrGenericFormat);
}



