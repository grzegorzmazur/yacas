/** \file lispenvironment.h
 *  General environment access.
 *
 */

#ifndef YACAS_LISPENVIRONMENT_H
#define YACAS_LISPENVIRONMENT_H

#include "yacasbase.h"
#include "lispobject.h"
#include "lisphash.h"
#include "lispevalhash.h"
#include "lispuserfunc.h"
#include "deffile.h"
#include "lispio.h"
#include "stringio.h"
#include "lispglobals.h"
#include "xmltokenizer.h"
#include "errors.h"
#include "noncopyable.h"

#include <atomic>
#include <string>
#include <sstream>
#include <vector>

#include <unordered_set>

typedef std::unordered_set<LispStringSmartPtr, std::hash<const LispString*> > LispIdentifiers;


class LispDefFiles;

class LispInput;
class LispOutput;
class LispPrinter;
class LispOperators;
class LispUserFunction;
class LispMultiUserFunction;
class LispEvaluatorBase;
class BasicEvaluator;
class DefaultDebugger;
class LispEnvironment;


/// The Lisp environment.
/// This huge class is the central class of the Yacas program. It
/// implements a dialect of Lisp.

class LispEnvironment : public YacasBase, NonCopyable
{
public:
  /// \name Constructor and destructor
  //@{
  LispEnvironment(YacasCoreCommands &aCoreCommands,
                  LispUserFunctions& aUserFunctions,
                  LispGlobal& aGlobals,
                  LispHashTable& aHashTable,
                  std::ostream& aOutput,
                  LispPrinter& aPrinter,
                  LispOperators &aPreFixOperators,
                  LispOperators &aInFixOperators,
                  LispOperators &aPostFixOperators,
                  LispOperators &aBodiedOperators,
                  LispIdentifiers& protected_symbols,
                  LispInput*    aCurrentInput,
                  LispInt aStackSize);
  ~LispEnvironment();
  //@}

public:
  /// \name Lisp variables
  //@{

  /// Assign a value to a Lisp variable.
  /// \param aString name of the variable
  /// \param aValue value to be assigned to \a aString
  ///
  /// If there is a local variable with the name \a aString, the
  /// object \a aValue is assigned to it. Otherwise, a
  /// LispGlobalVariable is constructed, and it is associated with
  /// \a aValue in #iGlobals.
  /// \sa FindLocal
  void SetVariable(const LispString* aString, LispPtr& aValue, bool aGlobalLazyVariable);

  /// In debug mode, DebugModeVerifySettingGlobalVariables raises a warning if a global variable is set.
  void DebugModeVerifySettingGlobalVariables(LispPtr & aVariable, bool aGlobalLazyVariable);

  /// Get the value assigned to a variable.
  /// \param aVariable name of the variable
  /// \param aResult (on exit) value of \a aVariable
  ///
  /// - If there is a local variable with the name \a aString,
  ///   \a aResult is set to point to the value assigned to this local
  ///   variable.
  /// - If there is a global variable \a aString and its
  ///   #iEvalBeforeReturn is false, its value is returned via
  ///   \a aResult.
  /// - If there is a global variable \a aString and its
  ///   #iEvalBeforeReturn is true, its value is evaluated. The
  ///   result is assigned back to the variable, its
  ///   #iEvalBeforeReturn is set to false, and a copy of the result
  ///   is returned in \a aResult.
  /// - Otherwise, \a aResult is set to #nullptr.
  void GetVariable(const LispString* aVariable, LispPtr& aResult);

  void UnsetVariable(const LispString * aString);
  void PushLocalFrame(bool aFenced);
  void PopLocalFrame();
  void NewLocal(const LispString* aVariable, LispObject* aValue);
  void CurrentLocals(LispPtr& aResult);
  //@}

public:
  /// \name Lisp functions
  //@{

  /// Return the #iCoreCommands attribute.
  inline YacasCoreCommands& CoreCommands();

  /// Add a command to the list of core commands.
  /// \param aEvaluatorFunc C function evaluating the core command
  /// \param aString name of the command
  /// \param aNrArgs number of arguments
  /// \param aFlags flags, see YacasEvaluator::FunctionFlags
  void SetCommand(YacasEvalCaller aEvaluatorFunc, const LispChar * aString,LispInt aNrArgs,LispInt aFlags);

  void RemoveCommand(LispChar * aString);
  void RemoveCoreCommand(LispChar * aString);

  inline  LispHashTable& HashTable();
  LispUserFunction* UserFunction(LispPtr& aArguments);
  LispUserFunction* UserFunction(const LispString* aName,LispInt aArity);

  /// Return LispMultiUserFunction with given name.
  /// \param aArguments name of the multi user function
  ///
  /// The table of user functions, #iUserFunctions, is consulted. If
  /// a user function with the given name exists, it is returned.
  /// Otherwise, a new LispMultiUserFunction is constructed, added
  /// to #iUserFunctions, and returned.
  LispMultiUserFunction* MultiUserFunction(const LispString* aArguments);

  LispDefFiles& DefFiles();
  void DeclareRuleBase(const LispString* aOperator, LispPtr& aParameters,
                       LispInt aListed);
  void DeclareMacroRuleBase(const LispString* aOperator, LispPtr& aParameters,
                       LispInt aListed);
  void DefineRule(const LispString* aOperator,LispInt aArity,
                          LispInt aPrecedence, LispPtr& aPredicate,
                          LispPtr& aBody);
  void DefineRulePattern(const LispString* aOperator,LispInt aArity,
                         LispInt aPrecedence, LispPtr& aPredicate,
                         LispPtr& aBody);


  void UnFenceRule(const LispString* aOperator,LispInt aArity);
  void Retract(const LispString* aOperator,LispInt aArity);
  void HoldArgument(const LispString* aOperator, const LispString* aVariable);

  void Protect(const LispString*);
  void UnProtect(const LispString*);
  bool Protected(const LispString*) const;
  //@}

public:
  /// \name Precision
  //@{

  /// set precision to a given number of decimal digits
  void SetPrecision(LispInt aPrecision);
  LispInt Precision(void) const;
  LispInt BinaryPrecision(void) const;
  //@}

public:
  void SetPrettyPrinter(const LispString* aPrettyPrinter);
  const LispString* PrettyPrinter();

  void SetPrettyReader(const LispString* aPrettyReader);
  const LispString* PrettyReader();

public:
  LispInt GetUniqueId();
public:
  LispPrinter& CurrentPrinter();

public:
  /// \name Operators
  //@{
  LispOperators& PreFix();
  LispOperators& InFix();
  LispOperators& PostFix();
  LispOperators& Bodied();
  //@}

public:
  /// \name Input and output
  //@{
  LispInput* CurrentInput();
  void SetCurrentInput(LispInput* aInput);
public:
  std::ostream& CurrentOutput();
  void SetCurrentOutput(std::ostream&);
  //@}

protected:
  /// current precision for user interaction, in decimal and in binary
  LispInt iPrecision;
  LispInt iBinaryPrecision;
public:
  std::vector<std::string> iInputDirectories;
  //DeletingLispCleanup iCleanup;
  LispInt iEvalDepth;
  LispInt iMaxEvalDepth;
#ifdef YACAS_NO_ATOMIC_TYPES
  volatile bool
#else
  std::atomic_bool
#endif // YACAS_NO_ATOMIC_TYPES
    stop_evaluation;
  LispEvaluatorBase* iEvaluator;

public: // Error information when some error occurs.
  InputStatus iInputStatus;
  bool secure;
public: // pre-found
  RefPtr<LispObject> iTrue;
  RefPtr<LispObject> iFalse;

  RefPtr<LispObject> iEndOfFile;
  RefPtr<LispObject> iEndStatement;
  RefPtr<LispObject> iProgOpen;
  RefPtr<LispObject> iProgClose;
  RefPtr<LispObject> iNth;
  RefPtr<LispObject> iBracketOpen;
  RefPtr<LispObject> iBracketClose;
  RefPtr<LispObject> iListOpen;
  RefPtr<LispObject> iListClose;
  RefPtr<LispObject> iComma;
  RefPtr<LispObject> iList;
  RefPtr<LispObject> iProg;

  LispInt iLastUniqueId;

public: // Error reporting
  std::ostringstream iErrorOutput;
  DefaultDebugger* iDebugger;

private:
    LispPtr *FindLocal(const LispString * aVariable);

    struct LispLocalVariable {
        LispLocalVariable(const LispString* var, LispObject* val):
        var(var), val(val)
        {
            ++var->iReferenceCount;
        }

        LispLocalVariable(const LispLocalVariable& v):
        var(v.var), val(v.val)
        {
            ++var->iReferenceCount;
        }

        ~LispLocalVariable()
        {
            --var->iReferenceCount;
        }

        const LispString* var;
        LispPtr val;
    };

    struct LocalVariableFrame {
        LocalVariableFrame(std::size_t first, bool fenced):
        first(first), fenced(fenced)
        {
        }

        std::size_t first;
        bool fenced;
    };

    std::vector<LispLocalVariable> _local_vars;
    std::vector<LocalVariableFrame> _local_frames;

public:
  std::ostream* iInitialOutput;

private:
  /// Hash of core commands with associated YacasEvaluator
  YacasCoreCommands& iCoreCommands;

  LispUserFunctions& iUserFunctions;
  LispHashTable& iHashTable;
  LispDefFiles   iDefFiles;
  LispPrinter&   iPrinter;
  std::ostream*    iCurrentOutput;

  /// Hash of global variables with their values
  LispGlobal&    iGlobals;

  LispOperators& iPreFixOperators;
  LispOperators& iInFixOperators;
  LispOperators& iPostFixOperators;
  LispOperators& iBodiedOperators;

  LispIdentifiers& protected_symbols;

  LispInput* iCurrentInput;

  const LispString* iPrettyReader;
  const LispString* iPrettyPrinter;
public:
  LispTokenizer iDefaultTokenizer;
  XmlTokenizer  iXmlTokenizer;
  LispTokenizer* iCurrentTokenizer;

public:
  /** YacasArgStack implements a stack of pointers to objects that can be used to pass
  *  arguments to functions, and receive results back.
  */
  class YacasArgStack {
  public:
    explicit YacasArgStack(std::size_t aStackSize):
      iStackCnt(0)
    {
      iStack.resize( aStackSize );
    }

    std::size_t GetStackTop() const
    {
        return iStackCnt;
    }

    void PushArgOnStack(LispObject* aObject)
    {
      if (iStackCnt >= iStack.size())
        RaiseStackOverflowError();

      iStack[iStackCnt++] = aObject;
    }

    LispPtr& GetElement(std::size_t aPos)
    {
      assert(aPos<iStackCnt);
      return iStack[aPos];
    }

    void PopTo(std::size_t aTop)
    {
      assert(aTop<=iStackCnt);
      iStackCnt=aTop;
    }

  private:
    void RaiseStackOverflowError() const
    {
      throw LispErrGeneric("Argument stack reached maximum. Please extend argument stack with --stack argument on the command line.");
    }

    // Invariants:
    //    0 <= iStackCnt <= iStack.Size()
    std::vector<LispPtr> iStack;
    std::size_t iStackCnt;    // number of items on the stack
  };

  YacasArgStack iStack;
};

inline LispInt LispEnvironment::Precision(void) const
{
    return iPrecision;
}

inline LispInt LispEnvironment::BinaryPrecision(void) const
{
  return iBinaryPrecision;
}



inline YacasCoreCommands& LispEnvironment::CoreCommands()
{
    return iCoreCommands;
}


inline LispHashTable& LispEnvironment::HashTable()
{
    return iHashTable;
}



// Local lisp stack, unwindable by the exception handler
class LispLocalFrame
{
public:
    LispLocalFrame(LispEnvironment& aEnvironment, bool aFenced)
        : iEnvironment(aEnvironment)
    {
        iEnvironment.PushLocalFrame(aFenced);
    };

    virtual ~LispLocalFrame()
    {
        iEnvironment.PopLocalFrame();
    };

private:
    LispEnvironment& iEnvironment;
};



class LispSecureFrame
{
public:
  LispSecureFrame(LispEnvironment& aEnvironment):
      iEnvironment(aEnvironment), previous_secure(aEnvironment.secure)
  {
    iEnvironment.secure = true;
  };
  virtual ~LispSecureFrame()
  {
    iEnvironment.secure = previous_secure;
  };
private:
  LispEnvironment& iEnvironment;
  bool previous_secure;
};


// LispLocalInput takes ownership over the LispInput class
class LispLocalInput: NonCopyable
{
public:
  LispLocalInput(LispEnvironment& aEnvironment, LispInput* aInput)
      : iEnvironment(aEnvironment),iPreviousInput(iEnvironment.CurrentInput())
  {
    iEnvironment.SetCurrentInput(aInput);
  };
  virtual ~LispLocalInput()
  {
    iEnvironment.SetCurrentInput(iPreviousInput);
  };

private:
  LispEnvironment& iEnvironment;
  LispInput* iPreviousInput;
};


// LispLocalInput takes ownership over the LispInput class
class LispLocalOutput: NonCopyable
{
public:
  LispLocalOutput(LispEnvironment& aEnvironment, std::ostream& aOutput)
      : iEnvironment(aEnvironment), iPreviousOutput(&iEnvironment.CurrentOutput())
  {
    iEnvironment.SetCurrentOutput(aOutput);
  };
  virtual ~LispLocalOutput()
  {
    iEnvironment.SetCurrentOutput(*iPreviousOutput);
  };

private:
  LispEnvironment& iEnvironment;
  std::ostream* iPreviousOutput;
};

class LispLocalEvaluator: NonCopyable
{
public:
  LispLocalEvaluator(LispEnvironment& aEnvironment,LispEvaluatorBase* aNewEvaluator);
  ~LispLocalEvaluator();

private:
  LispEvaluatorBase* iPreviousEvaluator;
  LispEnvironment& iEnvironment;
};

class LispLocalTrace: NonCopyable
{
public:
  LispLocalTrace(LispUserFunction* aUserFunc);
  ~LispLocalTrace();
private:
  LispUserFunction* iUserFunc;
};

inline void LispEnvironment::SetPrettyReader(const LispString* aPrettyReader)
{
  iPrettyReader = aPrettyReader;
}
inline const LispString* LispEnvironment::PrettyReader()
{
  return iPrettyReader;
}

inline void LispEnvironment::SetPrettyPrinter(const LispString * aPrettyPrinter)
{
  iPrettyPrinter = aPrettyPrinter;
}
inline const LispString* LispEnvironment::PrettyPrinter()
{
  return iPrettyPrinter;
}


#endif
