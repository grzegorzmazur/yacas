#ifndef YACAS_LISPERROR_H
#define YACAS_LISPERROR_H

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
    KLispErrGenericFormat,
    KLispNrErrors
};


template<typename T>
inline void Check(T hastobetrue, LispInt aError)
{
    if (!hastobetrue)
        throw aError;
}

class LispEnvironment;
class LispOutput;

void Handle(LispInt aError, LispEnvironment& aEnvironment, LispOutput& aOutput);


#ifdef YACAS_DEBUG
#define DBG_printf printf
#define DBG_(xxx) xxx
#else
namespace{void inline noop(...) {}}
// could change 'noop' to 'sizeof' below, but we get 'left-hand operand of comma has no effect' from g++
#define DBG_printf while (0) noop
#define DBG_(xxx) /*xxx*/
#endif


#endif

