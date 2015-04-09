#ifndef YACAS_LISPERROR_H
#define YACAS_LISPERROR_H

#include "lisptype.h"
#include "lispstring.h"

#include "choices.h"

#include <string>

class LispError {
public:
    LispError(const std::string& msg);

    const char* what() const;

private:
    const std::string _what;
};

inline
LispError::LispError(const std::string& what):
    _what(what)
{
}

inline
const char* LispError::what() const
{
    return _what.c_str();
}

class LispErrInvalidArg: public LispError {
public:
    LispErrInvalidArg():
        LispError("Invalid argument") {}
};

class LispErrWrongNumberOfArgs: public LispError {
public:
    LispErrWrongNumberOfArgs():
        LispError("Wrong number of arguments") {}
};

class LispErrNotList: public LispError {
public:
    LispErrNotList():
        LispError("Argument is not a list") {}
};

class LispErrListNotLongEnough: public LispError {
public:
    LispErrListNotLongEnough():
        LispError("List not long enough") {}
};

class LispErrInvalidStack: public LispError {
public:
    LispErrInvalidStack():
        LispError("Invalid stack") {}
};

class Quitting: public LispError {
public:
    Quitting():
        LispError("Quitting...") {}
};

class LispErrNotEnoughMemory: public LispError {
public:
    LispErrNotEnoughMemory():
        LispError("Not enough memory") {}
};

class InvalidToken: public LispError {
public:
    InvalidToken():
        LispError("Empty token during parsing") {}
};

class LispErrInvalidExpression: public LispError {
public:
    LispErrInvalidExpression():
        LispError("Error parsing expression") {}
};

class LispErrUnprintableToken: public LispError {
public:
    LispErrUnprintableToken():
        LispError("Unprintable atom") {}
};

class LispErrFileNotFound: public LispError {
public:
    LispErrFileNotFound():
        LispError("File not found") {}
};

class LispErrReadingFile: public LispError {
public:
    LispErrReadingFile():
        LispError("Error reading file") {}
};

class LispErrCreatingUserFunction: public LispError {
public:
    LispErrCreatingUserFunction():
        LispError("Could not create user function") {}
};

class LispErrCreatingRule: public LispError {
public:
    LispErrCreatingRule():
        LispError("Could not create rule") {}
};

class LispErrArityAlreadyDefined: public LispError {
public:
    LispErrArityAlreadyDefined():
        LispError("Rule base with this arity already defined") {}
};

class LispErrCommentToEndOfFile: public LispError {
public:
    LispErrCommentToEndOfFile():
        LispError("Reaching end of file within a comment block") {}
};

class LispErrNotString: public LispError {
public:
    LispErrNotString():
        LispError("Argument is not a string") {}
};

class LispErrNotInteger: public LispError {
public:
    LispErrNotInteger():
        LispError("Argument is not an integer") {}
};

class LispErrParsingInput: public LispError {
public:
    LispErrParsingInput():
        LispError("Error while parsing input") {}
};

class LispErrMaxRecurseDepthReached: public LispError {
public:
    LispErrMaxRecurseDepthReached():
        LispError("Max evaluation stack depth reached.\nPlease use MaxEvalDepth to increase the stack size as needed.") {}
};

class LispErrDefFileAlreadyChosen: public LispError {
public:
    LispErrDefFileAlreadyChosen():
        LispError("DefFile already chosen for function") {}
};

class LispErrDivideByZero: public LispError {
public:
    LispErrDivideByZero():
        LispError("Divide by zero") {}
};

class LispErrNotAnInFixOperator: public LispError {
public:
    LispErrNotAnInFixOperator():
        LispError("Trying to make a non-infix operator right-associative") {}
};

class LispErrUser: public LispError {
public:
    LispErrUser(const std::string& msg):
        LispError(msg) {}
};

class LispErrIsNotInFix: public LispError {
public:
    LispErrIsNotInFix():
        LispError("Trying to get precedence of non-infix operator") {}
};

class LispErrSecurityBreach: public LispError {
public:
    LispErrSecurityBreach():
        LispError("Trying to perform an insecure action") {}
};

class LispErrLibraryNotFound: public LispError {
public:
    LispErrLibraryNotFound():
        LispError("Could not find library") {}
};

class LispErrUserInterrupt: public LispError {
public:
    LispErrUserInterrupt():
        LispError("User interrupted calculation") {}
};

class LispErrNonBooleanPredicateInPattern: public LispError {
public:
    LispErrNonBooleanPredicateInPattern():
        LispError("Predicate doesn't evaluate to a boolean in pattern") {}
};

class LispErrProtectedSymbol: public LispError {
public:
    explicit LispErrProtectedSymbol(const std::string& s):
        LispError(std::string("Attempt to override protected symbol: ") + s) {}
};

class LispErrGeneric: public LispError {
public:
    LispErrGeneric(const std::string& what):
        LispError(what) {}
};

class LispEnvironment;
class LispOutput;

void HandleError(const LispError&, LispEnvironment& aEnvironment, std::ostream& aOutput);


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

