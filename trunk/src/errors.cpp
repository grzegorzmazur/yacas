
#include "yacasprivate.h"
#include "lispenvironment.h"
#include "standard.h"
#include "lispeval.h"
#include "errors.h"
#include "infixparser.h"

#ifdef HAVE_STDIO_H
  #include <stdio.h>
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
  if (aArguments.Get() == NULL)
  {
    aEnvironment.iErrorOutput.Write("Error in compiled code\n");
  }
  else
  {
    LispStringPtr string = aArguments.Get()->String();
    if (string)
    {
      aEnvironment.iErrorOutput.Write("In function \"");
      aEnvironment.iErrorOutput.Write(string->String());
      aEnvironment.iErrorOutput.Write("\" : \n");
    }
  }
}

void CheckNrArgs(LispInt n, LispPtr& aArguments,
                 LispEnvironment& aEnvironment)
{
    LispInt nrArguments = InternalListLength(aArguments);
    if (nrArguments != n)
    {
        ErrorNrArgs(n-1, nrArguments-1, aArguments, aEnvironment);
    }
}

void ErrorNrArgs(LispInt needed, LispInt passed, LispPtr& aArguments,
                 LispEnvironment& aEnvironment)
{
  if (aArguments.Get() == NULL)
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


void CheckFuncGeneric(LispInt aPredicate,LispInt aError,LispPtr& aArguments,
                      LispEnvironment& aEnvironment)
{
  if (!aPredicate)
  {
    if (aArguments.Get() == NULL)
    {
      aEnvironment.iErrorOutput.Write("Error in compiled code\n");
    }
    else
    {
      ShowStack(aEnvironment);
      ShowFunctionError(aArguments, aEnvironment);
      Check(aPredicate,aError);
    }
  }
}
void CheckFuncGeneric(LispInt aPredicate,LispInt aError,
                      LispEnvironment& aEnvironment)
{
  if (!aPredicate)
  {
    ShowStack(aEnvironment);
    Check(aPredicate,aError);
  }
}

/*TODO remove??? */
void CheckArgType(LispInt aPredicate, LispInt aArgNr, LispPtr& aArguments,
                  LispEnvironment& aEnvironment)
{
    CheckArgType(aPredicate, aArgNr, aArguments,
                 aEnvironment, KLispErrInvalidArg);
}
/* */

void CheckArgType(LispInt aPredicate, LispInt aArgNr, LispPtr& aArguments,LispEnvironment& aEnvironment,
                  LispInt aError)
{
    if (!aPredicate)
    {
        if (aArguments.Get() == NULL)
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
          aEnvironment.iErrorOutput.Write(strout.String());
  //        aEnvironment.iErrorOutput.Write("\n");
  
  //        aEnvironment.iErrorOutput.Write("Argument ");
  //        PrintExpression(strout, arg, aEnvironment, LIM_AL);
  //        aEnvironment.iErrorOutput.Write(strout.String());
  
          LispPtr eval;
          InternalEval(aEnvironment, eval, arg);
          aEnvironment.iErrorOutput.Write(" evaluated to ");
          PrintExpression(strout, eval, aEnvironment, LIM_AL);
          aEnvironment.iErrorOutput.Write(strout.String());
          aEnvironment.iErrorOutput.Write("\n");
  
#ifdef YACAS_DEBUG
          printf("Problem occurred at %s(%d)\n",
                aArguments.Get()->iFileName,
                aArguments.Get()->iLine
                );
#endif
        }        
        Check(aPredicate,aError);
    }
}




char theGenericErrorBuf[512];
char *GenericErrorBuf()
{
  return theGenericErrorBuf;
}

void RaiseError(char* str,...)
{
#ifdef HAVE_STDARG_H
  va_list arg;
  va_start (arg, str);
 #ifdef HAVE_VSNPRINTF
  vsnprintf (theGenericErrorBuf, 500, str, arg);
 #else
  /* Just cross fingers and hope the buffer is large enough */
  vsprintf (theGenericErrorBuf, str, arg);
 #endif  
  va_end (arg);
#else
  PlatMemCopy(theGenericErrorBuf, str, PlatStrLen(str));
#endif
  Check(LispFalse,KLispErrGenericFormat);
}


