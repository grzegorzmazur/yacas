
#ifndef __errors_h__
#define __errors_h__


void ShowFunctionError(LispPtr& aArguments,
                       LispEnvironment& aEnvironment);
void CheckNrArgs(LispInt n, LispPtr& aArguments,
                 LispEnvironment& aEnvironment);
void ErrorNrArgs(LispInt needed, LispInt passed, LispPtr& aArguments,
                 LispEnvironment& aEnvironment);

void CheckFuncGeneric(LispInt aPredicate,LispInt aError,LispPtr& aArguments,
                      LispEnvironment& aEnvironment);
void CheckFuncGeneric(LispInt aPredicate,LispInt aError,
                      LispEnvironment& aEnvironment);

/*TODO remove??? */
void CheckArgType(LispInt aPredicate, LispInt aArgNr, LispPtr& aArguments,LispEnvironment& aEnvironment);
/* */
void CheckArgType(LispInt aPredicate, LispInt aArgNr, LispPtr& aArguments,LispEnvironment& aEnvironment,
                  LispInt aError);

#define CHK_ARG(_pred,_argnr) {if (!(int)(_pred)) CheckArgType(0,_argnr,aArguments,aEnvironment);}

#define TESTARGS(_n)  CheckNrArgs(_n,aArguments,aEnvironment)

#define CHK(_pred,_err) {if (!(int)(_pred)) CheckFuncGeneric(0,_err,aArguments,aEnvironment);}
#define CHK2(_pred,_err) {if (!(int)(_pred)) CheckFuncGeneric(0,_err,aEnvironment);}

#define CHK_ISLIST(_pred, _argnr) {if (!InternalIsList(_pred)) CheckArgType(0 ,_argnr, aArguments,aEnvironment,KLispErrNotList);}


#define CHK_ISSTRING(_pred, _argnr) {if (!InternalIsString((_pred).Get()->String())) CheckArgType(0 ,_argnr, aArguments,aEnvironment,KLispErrNotString);}

/*
#define TESTARGS(_n)  \
    { \
      LispInt nrArguments = InternalListLength(aArguments);  \
      Check(nrArguments == _n,KLispErrWrongNumberOfArgs); \
    }
*/

#endif
