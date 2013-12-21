#ifndef YACAS_ERRORS_H
#define YACAS_ERRORS_H

#ifdef __GNUC__
#  define YACAS_NORETURN __attribute__((noreturn))
#else
#  define YACAS_NORETURN
#endif

void CheckArgType(LispInt aArgNr, LispPtr& aArguments, LispEnvironment& aEnvironment, LispInt aError) YACAS_NORETURN;
#define CHK_ARG(_pred,_argnr)           {if (!(_pred))                              CheckArgType(_argnr,aArguments, aEnvironment,KLispErrInvalidArg);}
#define CHK_ARG_CORE(_pred,_argnr)      {if (!(_pred))                              CheckArgType(_argnr,ARGUMENT(0),aEnvironment,KLispErrInvalidArg);}
#define CHK_ISLIST(_pred,_argnr)        {if (!InternalIsList(_pred))                     CheckArgType(_argnr,aArguments, aEnvironment,KLispErrNotList);}
#define CHK_ISLIST_CORE(_pred,_argnr)   {if (!InternalIsList(_pred))                     CheckArgType(_argnr,ARGUMENT(0),aEnvironment,KLispErrNotList);}
#define CHK_ISSTRING(_pred,_argnr)      {if (!InternalIsString((_pred)->String())) CheckArgType(_argnr,aArguments, aEnvironment,KLispErrNotString);}
#define CHK_ISSTRING_CORE(_pred,_argnr) {if (!InternalIsString((_pred)->String())) CheckArgType(_argnr,ARGUMENT(0),aEnvironment,KLispErrNotString);}

void CheckNrArgs(LispInt n, LispPtr& aArguments, LispEnvironment& aEnvironment);

void CheckFuncGeneric(LispInt aError,LispPtr& aArguments,LispEnvironment& aEnvironment) YACAS_NORETURN;
void CheckFuncGeneric(LispInt aError,LispEnvironment& aEnvironment) YACAS_NORETURN;

#define CHK(_pred,_err)      {if (!(_pred)) CheckFuncGeneric(_err,aArguments, aEnvironment);}
#define CHK_CORE(_pred,_err) {if (!(_pred)) CheckFuncGeneric(_err,ARGUMENT(0),aEnvironment);}
#define CHK2(_pred,_err)     {if (!(_pred)) CheckFuncGeneric(_err,aEnvironment);}

char *GenericErrorBuf(); // called (only) from errors.cpp and LispEnvironment::ErrorString()
void RaiseError(const char* str,...);

#endif
