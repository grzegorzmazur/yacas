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
#include "lispcleanupstack.h"
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
  void SetVariable(LispString * aString, LispPtr& aValue, bool aGlobalLazyVariable);

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
  void GetVariable(LispString * aVariable,LispPtr& aResult);

  void UnsetVariable(LispString * aString);
  void PushLocalFrame(bool aFenced);
  void PopLocalFrame();
  void NewLocal(LispString * aVariable,LispObject* aValue);
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
  LispUserFunction* UserFunction(LispString * aName,LispInt aArity);

  /// Return LispMultiUserFunction with given name.
  /// \param aArguments name of the multi user function
  ///
  /// The table of user functions, #iUserFunctions, is consulted. If
  /// a user function with the given name exists, it is returned.
  /// Otherwise, a new LispMultiUserFunction is constructed, added
  /// to #iUserFunctions, and returned.
  LispMultiUserFunction* MultiUserFunction(LispString * aArguments);

  LispDefFiles& DefFiles();
  void DeclareRuleBase(LispString * aOperator, LispPtr& aParameters,
                       LispInt aListed);
  void DeclareMacroRuleBase(LispString * aOperator, LispPtr& aParameters,
                       LispInt aListed);
  void DefineRule(LispString * aOperator,LispInt aArity,
                          LispInt aPrecedence, LispPtr& aPredicate,
                          LispPtr& aBody);
  void DefineRulePattern(LispString * aOperator,LispInt aArity,
                          LispInt aPrecedence, LispPtr& aPredicate,
                          LispPtr& aBody);


  void UnFenceRule(LispString * aOperator,LispInt aArity);
  void Retract(LispString * aOperator,LispInt aArity);
  void HoldArgument(LispString *  aOperator,LispString * aVariable);
  //@}

  LispString * FindCachedFile(const LispChar * aFileName);

public:
  /// \name Precision
  //@{

  /// set precision to a given number of decimal digits
  void SetPrecision(LispInt aPrecision);
  inline LispInt Precision(void) const;
  inline LispInt BinaryPrecision(void) const;
  //@}

public:
  inline void SetPrettyPrinter(LispString * aPrettyPrinter);
  inline LispString * PrettyPrinter(void);

  inline void SetPrettyReader(LispString * aPrettyReader);
  inline LispString * PrettyReader(void);

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
  DeletingLispCleanup iCleanup;
  LispInt iEvalDepth;
  LispInt iMaxEvalDepth;
  std::atomic_bool stop_evaluation;
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
  LispPtr *FindLocal(LispString * aVariable);

private:

  class LispLocalVariable : public YacasBase, NonCopyable
  {
  public:
    LispLocalVariable(LispString * aVariable,
                      LispObject* aValue)
      : iNext(nullptr), iVariable(aVariable),iValue(aValue)
    {
      ++aVariable->iReferenceCount;
    };
    ~LispLocalVariable()
    {
      --iVariable->iReferenceCount;
    }

  public:
    LispLocalVariable* iNext;
    LispString * iVariable;
    LispPtr iValue;
  };
  class LocalVariableFrame : public YacasBase, NonCopyable
  {
  public:
    LocalVariableFrame(LocalVariableFrame *aNext,
                       LispLocalVariable* aFirst)
        : iNext(aNext), iFirst(aFirst), iLast(aFirst) { }
    void Add(LispLocalVariable* aNew)
    {
      aNew->iNext = iFirst;
      iFirst = aNew;
    }
    ~LocalVariableFrame()
    {
      LispLocalVariable* t = iFirst;
      LispLocalVariable* next;
      while (t != iLast)
      {
        next = t->iNext;
        delete t;
        t = next;
      }
    }

  public:
    LocalVariableFrame *iNext;
    LispLocalVariable* iFirst;
    LispLocalVariable* iLast;
  };

private:
  LocalVariableFrame *iLocalsList;

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

  LispInput* iCurrentInput;

  LispString * iPrettyReader;
  LispString * iPrettyPrinter;
public:
  LispTokenizer iDefaultTokenizer;
  CommonLispTokenizer iCommonLispTokenizer;
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
class LispLocalFrame : public LispBase
{
public:
    LispLocalFrame(LispEnvironment& aEnvironment, bool aFenced)
        : iEnvironment(aEnvironment)
    {
        iEnvironment.PushLocalFrame(aFenced);
        SAFEPUSH(iEnvironment,*this);
    };
    virtual ~LispLocalFrame()
    {
        SAFEPOP(iEnvironment);
        Delete();
    };
    virtual void Delete();
private:
    LispEnvironment& iEnvironment;
};



class LispSecureFrame : public LispBase
{
public:
  LispSecureFrame(LispEnvironment& aEnvironment):
      iEnvironment(aEnvironment), previous_secure(aEnvironment.secure)
  {
    iEnvironment.secure = true;
    SAFEPUSH(iEnvironment,*this);
  };
  virtual ~LispSecureFrame()
  {
    SAFEPOP(iEnvironment);
    Delete();
  };
  virtual void Delete();
private:
  LispEnvironment& iEnvironment;
  bool previous_secure;
};


// LispLocalInput takes ownership over the LispInput class
class LispLocalInput : public LispBase, NonCopyable
{
public:
  LispLocalInput(LispEnvironment& aEnvironment, LispInput* aInput)
      : iEnvironment(aEnvironment),iPreviousInput(iEnvironment.CurrentInput())
  {
    iEnvironment.SetCurrentInput(aInput);
    SAFEPUSH(iEnvironment,*this);
  };
  virtual ~LispLocalInput()
  {
    SAFEPOP(iEnvironment);
    Delete();
  };
  virtual void Delete();

private:
  LispEnvironment& iEnvironment;
  LispInput* iPreviousInput;
};


// LispLocalInput takes ownership over the LispInput class
class LispLocalOutput : public LispBase, NonCopyable
{
public:
  LispLocalOutput(LispEnvironment& aEnvironment, std::ostream& aOutput)
      : iEnvironment(aEnvironment), iPreviousOutput(&iEnvironment.CurrentOutput())
  {
    iPreviousOutput = &iEnvironment.CurrentOutput();
    iEnvironment.SetCurrentOutput(aOutput);
    SAFEPUSH(iEnvironment,*this);
  };
  virtual ~LispLocalOutput()
  {
    SAFEPOP(iEnvironment);
    Delete();
  };
  virtual void Delete();

private:
  LispEnvironment& iEnvironment;
  std::ostream* iPreviousOutput;
};

class LispLocalEvaluator : public YacasBase, NonCopyable
{
public:
  LispLocalEvaluator(LispEnvironment& aEnvironment,LispEvaluatorBase* aNewEvaluator);
  ~LispLocalEvaluator();

private:
  LispEvaluatorBase* iPreviousEvaluator;
  LispEnvironment& iEnvironment;
};

class LispLocalTrace : public YacasBase, NonCopyable
{
public:
  LispLocalTrace(LispUserFunction* aUserFunc);
  ~LispLocalTrace();
private:
  LispUserFunction* iUserFunc;
};

class LocalArgs : public YacasBase, NonCopyable
{
public:
  LocalArgs(LispPtr* aPtrs) : iPtrs(aPtrs) {};
  ~LocalArgs()
  {
    if (iPtrs)
      delete[] iPtrs;
  }

private:
  LispPtr* iPtrs;
};



inline void LispEnvironment::SetPrettyReader(LispString * aPrettyReader)
{
  iPrettyReader = aPrettyReader;
}
inline LispString * LispEnvironment::PrettyReader(void)
{
  return iPrettyReader;
}

inline void LispEnvironment::SetPrettyPrinter(LispString * aPrettyPrinter)
{
  iPrettyPrinter = aPrettyPrinter;
}
inline LispString * LispEnvironment::PrettyPrinter(void)
{
  return iPrettyPrinter;
}


#endif