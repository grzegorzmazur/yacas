
#ifndef __lisperror_h__
#define __lisperror_h__

#include "lisptype.h"
#include "lispstring.h"

#include "choices.h"


enum ErrorCodes
{
    KLispErrNone = 0,
    KLispErrInvalidArg        ,
    KLispErrWrongNumberOfArgs ,
    KLispErrNotList           ,
    KLispErrListNotLongEnough ,
    KLispErrInvalidStack      ,
    KQuitting                 ,
    KLispErrNotEnoughMemory   ,
    KInvalidToken             ,
    KLispErrInvalidExpression ,
    KLispErrUnprintableToken  ,
    KLispErrFileNotFound      ,
    KLispErrReadingFile       ,
    KLispErrCreatingUserFunction,
    KLispErrCreatingRule        ,
    KLispErrArityAlreadyDefined ,
    KLispErrCommentToEndOfFile  ,
    KLispErrNotString           ,
    KLispErrNotInteger          ,
    KLispErrParsingInput        ,
    KLispErrMaxRecurseDepthReached,
    KLispErrDefFileAlreadyChosen  ,
    KLispErrDivideByZero          ,
    KLispErrNotAnInFixOperator    ,
    KLispErrUser                  ,
    KLispErrIsNotInFix            ,
    KLispErrSecurityBreach        ,
    KLispErrLibraryNotFound       ,
    KLispErrUserInterrupt         ,
    KLispErrNonBooleanPredicateInPattern,
    KLispNrErrors
};



inline void Check(LispBoolean hastobetrue, LispInt aError)
{
    if (!hastobetrue)
        LispThrow(aError); // TODO new LispException(aException)
}




#endif

