

#ifndef __yacas_h__
#define __yacas_h__

#include "lispstring.h"
#include "stringio.h"
#include "tokenizer.h"
#include "lisphash.h"
#include "lispevalhash.h"
#include "infixparser.h"
#include "stdfileio.h"
#include "lispatom.h"
#include "lispeval.h"
#include "mathenvironment.h"
#include "lispglobals.h"
#include "lisperror.h"
#include "lispuserfunc.h"


class DefaultYacasEnvironment
{
public:
    DefaultYacasEnvironment();
    DefaultYacasEnvironment(LispOutput* aOutput);
    virtual ~DefaultYacasEnvironment();
    LispEnvironment& operator() () {return iEnvironment;}
    void SetCommand(LispEvalCaller aEvaluatorFunc,LispCharPtr aString);
    
private:
    LispOutput* output;
    LispHashTable hash;
    LispPrinter printer;

    LispCommands commands;
    LispGlobal globals;

    //Define the default operators.
    LispOperators prefixoperators;
    LispOperators infixoperators;
    LispOperators postfixoperators;
    LispOperators bodiedoperators;
    InfixPrinter infixprinter;

    LispUserFunctions userFunctions;

    LispEnvironment iEnvironment;

public:
    CachedStdUserInput input;
};


class CYacas
{
public:
    LISPIMPORT static CYacas* NewL();
    LISPIMPORT static CYacas* NewL(LispOutput* aOutput);
    LISPIMPORT virtual ~CYacas();
    inline DefaultYacasEnvironment& operator()() {return environment;}
    virtual void Evaluate(LispCharPtr aExpression);
    virtual LispCharPtr Result();
    virtual LispCharPtr Error();
private:
  CYacas(LispOutput* aOutput);

private:
  DefaultYacasEnvironment environment;

  LispString iResult;

  StringOutput iResultOutput;
};


#endif


