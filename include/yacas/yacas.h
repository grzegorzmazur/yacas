/// \file
/// Definitions of DefaultYacasEnvironment and CYacas.

#ifndef YACAS_YACAS_H
#define YACAS_YACAS_H

#include "yacasbase.h"
#include "lispstring.h"
#include "stringio.h"
#include "tokenizer.h"
#include "lisphash.h"
#include "lispevalhash.h"
#include "infixparser.h"
#include "stdfileio.h"
#include "lispatom.h"
#include "lispeval.h"
#include "lispglobals.h"
#include "lisperror.h"
#include "lispuserfunc.h"
#include "noncopyable.h"

#include <sstream>


/// The default environment for a Yacas session.
/// This class constructs a LispEnvironment (to be found in
/// #iEnvironment), and defines the Yacas core functions. The core
/// functions are listed in corefunctions.h . Examples of core
/// functions are \c Head, \c Set and \c Eval.

class DefaultYacasEnvironment: NonCopyable
{
public:
  DefaultYacasEnvironment(std::ostream&, LispInt aStackSize);
  LispEnvironment& getEnv() {return iEnvironment;}

private:
  std::ostream& output;
  LispHashTable hash;
  LispPrinter printer;

  YacasCoreCommands coreCommands;
  LispGlobal globals;

  //Define the default operators.
  LispOperators prefixoperators;
  LispOperators infixoperators;
  LispOperators postfixoperators;
  LispOperators bodiedoperators;
  InfixPrinter infixprinter;

  LispUserFunctions userFunctions;

  LispIdentifiers protected_symbols;

  LispEnvironment iEnvironment;

public:
  StdUserInput input;
};


/// The Yacas engine.
/// This is the only class that applications need to use. It can
/// evaluate Yacas expressions. Every instance has its own Yacas
/// environment, in which the expressions are evaluated.



class CYacas {
public:
    /// Constructor
    LISPIMPORT CYacas(std::ostream&, LispInt aStackSize = 50000);

    /// Return the underlying Yacas environment.
    DefaultYacasEnvironment& getDefEnv() {return environment;}

    /// Evaluate a Yacas expression.
    /// First, \p aExpression is parsed by an InfixParser. Then it is
    /// evaluated in the underlying Lisp environment. Finally, the
    /// result is printed to #iResultOutput via the pretty printer or,
    /// if this is not defined, via an InfixPrinter.
    void Evaluate(const std::string& aExpression);

    /// Return the result of the expression.
    /// This is stored in #iResult.
    const std::string& Result() const;

    /// Return the error message produced by the last evaluation.
    /// The error is retrieved from #environment.
    const std::string& Error() const;

    /// Whether an error occured during the last evaluation.
    bool IsError() const;

private:

    /// The underlying Yacas environment
    DefaultYacasEnvironment environment;

    std::string _result;
    std::string _error;
};

inline
const std::string& CYacas::Result() const
{
  return _result;
}

inline
const std::string& CYacas::Error() const
{
  return _error;
}

inline
bool CYacas::IsError() const
{
    return !Error().empty();
}

#endif
