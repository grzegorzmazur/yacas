/** \file lispenvironment.h
 *  General environment access.
 *
 */


#ifndef __lispenvironment_h__
#define __lispenvironment_h__

#include "yacasbase.h"
#include "lispobject.h"
#include "lisphash.h"
#include "lispevalhash.h"
#include "lispcleanupstack.h"
#include "deffile.h"
#include "lispio.h"
#include "stringio.h"
#include "lispglobals.h"
#include "lispplugin.h"
#include "ctokenizer.h"
#include "xmltokenizer.h"
#include "errors.h"

class CCompressedArchive; /* defined in archiver.h */

class LispDefFiles;
class InputDirectories : public CDeletingArrayGrower<LispStringPtr>
{
};

class LispInput;
class LispOutput;
class LispPrinter;
class LispOperators;
class LispUserFunctions;
class LispUserFunction;
class LispMultiUserFunction;
class LispEvaluatorBase;
class BasicEvaluator;
class LispDllBase;
class DefaultDebugger;
class LispEnvironment;
class CDllArray : public CDeletingArrayGrower<LispDllBase*>
{
public:
    void DeleteNamed(LispCharPtr aName, LispEnvironment& aEnvironment);
};


/// The Lisp environment.
/// This huge class is the central class of the Yacas program. It
/// implements a dialect of Lisp.

class LispEnvironment : public YacasBase
{
public:
    /// \name Constructor and destructor
    //@{
    LispEnvironment(YacasCoreCommands &aCoreCommands,
                    LispUserFunctions& aUserFunctions,
                    LispGlobal& aGlobals,
                    LispHashTable& aHashTable,
                    LispOutput* aOutput,
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
    void SetVariable(LispStringPtr aString, LispPtr& aValue);

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
    /// - Otherwise, \a aResult is set to #NULL.
    void GetVariable(LispStringPtr aVariable,LispPtr& aResult);

    void SetGlobalEvaluates(LispStringPtr aVariable);

    void UnsetVariable(LispStringPtr aString);
    void PushLocalFrame(LispBoolean aFenced);
    void PopLocalFrame();
    void NewLocal(LispStringPtr aVariable,LispObject* aValue);
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
    void SetCommand(YacasEvalCaller aEvaluatorFunc, LispCharPtr aString,LispInt aNrArgs,LispInt aFlags);

    void RemoveCommand(LispCharPtr aString);
    void RemoveCoreCommand(LispCharPtr aString);

    inline  LispHashTable& HashTable();
    LispUserFunction* UserFunction(LispPtr& aArguments);
    LispUserFunction* UserFunction(LispStringPtr aName,LispInt aArity);

    /// Return LispMultiUserFunction with given name.
    /// \param aArguments name of the multi user function
    ///
    /// The table of user functions, #iUserFunctions, is consulted. If
    /// a user function with the given name exists, it is returned. 
    /// Otherwise, a new LispMultiUserFunction is constructed, added
    /// to #iUserFunctions, and returned. 
    LispMultiUserFunction* MultiUserFunction(LispStringPtr aArguments);

    LispDefFiles& DefFiles();
    void DeclareRuleBase(LispStringPtr aOperator, LispPtr& aParameters,
                         LispInt aListed);
    void DeclareMacroRuleBase(LispStringPtr aOperator, LispPtr& aParameters, 
                         LispInt aListed);
    void DefineRule(LispStringPtr aOperator,LispInt aArity,
                            LispInt aPrecedence, LispPtr& aPredicate,
                            LispPtr& aBody);
    void DefineRulePattern(LispStringPtr aOperator,LispInt aArity,
                            LispInt aPrecedence, LispPtr& aPredicate,
                            LispPtr& aBody);


    void UnFenceRule(LispStringPtr aOperator,LispInt aArity);
    void Retract(LispStringPtr aOperator,LispInt aArity);
    void HoldArgument(LispStringPtr  aOperator,LispStringPtr aVariable);
    //@}

    LispStringPtr FindCachedFile(LispCharPtr aFileName);
    
public:
    /// \name Precision
    //@{

    /// set precision to a given number of decimal digits
    void SetPrecision(LispInt aPrecision);
    inline LispInt Precision(void) const;
    inline LispInt BinaryPrecision(void) const;
    //@}

public:
    inline void SetPrettyPrinter(LispStringPtr aPrettyPrinter);
    inline LispStringPtr PrettyPrinter(void);
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
    LispOutput* CurrentOutput();
    void SetCurrentOutput(LispOutput* aOutput);
public:
    void SetUserError(LispCharPtr aErrorString);
    LispCharPtr ErrorString(LispInt aError);
    //@}

protected:
		/// current precision for user interaction, in decimal and in binary
    LispInt iPrecision;
	LispInt iBinaryPrecision;
public:
    InputDirectories iInputDirectories;
    InputDirectories iDllDirectories;
    DeletingLispCleanup iCleanup;
    LispInt iEvalDepth;
    LispInt iMaxEvalDepth;
    CCompressedArchive *iArchive;
    LispEvaluatorBase* iEvaluator;

public: // Error information when some error occurs.
    InputStatus iInputStatus;
    LispInt iSecure;
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
    LispString iError;
    StringOutput iErrorOutput;
    CDllArray iDlls;
    DefaultDebugger* iDebugger;
    
private:
    LispPtr *FindLocal(LispStringPtr aVariable);

private:

    class LispLocalVariable : public YacasBase
    {
    public:
        LispLocalVariable(LispStringPtr aVariable,
                          LispObject* aValue)
            : iNext(NULL), iVariable(aVariable),iValue(*aValue)
        {
            aVariable->IncreaseRefCount();
        };
        ~LispLocalVariable()
        {
            iVariable->DecreaseRefCount();
        }
    public:
        LispLocalVariable* iNext;
        LispStringPtr iVariable;
        LispPtr iValue;
    };
    class LocalVariableFrame : public YacasBase
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
public: //Well... only because I want to be able to show the stack to the outside world...
    LocalVariableFrame *iLocalsList;
    LispOutput*    iInitialOutput;

private:

    /// Hash of core commands with associated YacasEvaluator
    YacasCoreCommands& iCoreCommands;

    LispUserFunctions& iUserFunctions;
    LispHashTable& iHashTable;
    LispDefFiles   iDefFiles;
    LispPrinter&   iPrinter;
    LispOutput*    iCurrentOutput;

    /// Hash of global variables with their values
    LispGlobal&    iGlobals;

//    LispPtr        iLocals;

    LispOperators& iPreFixOperators;
    LispOperators& iInFixOperators;
    LispOperators& iPostFixOperators;
    LispOperators& iBodiedOperators;

    LispInput* iCurrentInput;

    LispCharPtr theUserError;

    LispStringPtr iPrettyPrinter;
public:
    LispTokenizer iDefaultTokenizer;
    CommonLispTokenizer iCommonLispTokenizer;
    CTokenizer    iCTokenizer;
    XmlTokenizer  iXmlTokenizer;
    LispTokenizer* iCurrentTokenizer;

public:
  /** YacasArgStack implements a stack of pointers to objects that can be used to pass
  *  arguments to functions, and receive results back.
  */
  class YacasArgStack
  {
  public:
    //TODO appropriate constructor?
    YacasArgStack(LispInt aStackSize) : iStack(aStackSize,NULL),iStackTop(0) 
    {
//printf("STACKSIZE %d\n",aStackSize);
    }
    inline LispInt GetStackTop() const {return iStackTop;}
    inline void PushArgOnStack(LispObject* aObject) 
    {
      if (iStackTop >= iStack.Size())
      {
        RaiseError("Argument stack reached maximum. Please extend argument stack with --stack argument on the command line.");
      }
      iStack.SetElement(iStackTop,aObject);
      iStackTop++;
    }
    inline LispPtr& GetElement(LispInt aPos) 
    {
      LISPASSERT(aPos>=0 && aPos < iStackTop);
      return iStack.GetElement(aPos);
    }
    inline void PopTo(LispInt aTop) 
    {
      LISPASSERT(aTop<=iStackTop);
      while (iStackTop>aTop)
      {
        iStackTop--;
        iStack.SetElement(iStackTop,NULL);
      }
    }
  protected:
    LispPtrArray iStack;
    LispInt iStackTop;
  };

  YacasArgStack iStack;
};

/* this function is now in lispenvironment.cpp
inline void LispEnvironment::SetPrecision(LispInt aPrecision)
{
    iPrecision = aPrecision;
	iBinaryPrecision = digits_to_bits(aPrecision, BASE10);
}
*/

inline LispInt LispEnvironment::Precision(void) const
{
    return iPrecision;
}

inline LispInt LispEnvironment::BinaryPrecision(void) const
{
//FIXME TODO need the right place for this function definition
//    extern unsigned long digits_to_bits(unsigned long digits, unsigned base);
//    return digits_to_bits(iPrecision,10);
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
    LispLocalFrame(LispEnvironment& aEnvironment, LispBoolean aFenced)
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
    LispSecureFrame(LispEnvironment& aEnvironment)
        : iEnvironment(aEnvironment)
    {
        iPreviousSecure = iEnvironment.iSecure;
        iEnvironment.iSecure = 1;
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
    LispInt iPreviousSecure;
};


// LispLocalInput takes ownership over the LispInput class
class LispLocalInput : public LispBase
{
public:
    LispLocalInput(LispEnvironment& aEnvironment, LispInput* aInput)
        : iEnvironment(aEnvironment)
    {
        iPreviousInput = iEnvironment.CurrentInput();
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
class LispLocalOutput : public LispBase
{
public:
    LispLocalOutput(LispEnvironment& aEnvironment, LispOutput* aOutput)
        : iEnvironment(aEnvironment)
    {
        iPreviousOutput = iEnvironment.CurrentOutput();
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
    LispOutput* iPreviousOutput;
};


class LispLocalEvaluator : public YacasBase
{
public:
    LispLocalEvaluator(LispEnvironment& aEnvironment,LispEvaluatorBase* aNewEvaluator);
    ~LispLocalEvaluator();
    
private:
    LispEvaluatorBase* iPreviousEvaluator;
    LispEnvironment& iEnvironment;
};

class LispLocalTrace : public YacasBase
{
public:
    LispLocalTrace(LispUserFunction* aUserFunc);
    ~LispLocalTrace();
private:
    LispUserFunction* iUserFunc;
};

class LocalArgs : public YacasBase
{
public:
    LocalArgs(LispPtr* aPtrs)
        : iPtrs(aPtrs)
    {};
    ~LocalArgs()
    {
        if (iPtrs)
            delete[] iPtrs;
    }
private:
    LispPtr* iPtrs;
};



inline void LispEnvironment::SetPrettyPrinter(LispStringPtr aPrettyPrinter)
{
    iPrettyPrinter = aPrettyPrinter;
}
inline LispStringPtr LispEnvironment::PrettyPrinter(void)
{
    return iPrettyPrinter;
}


#endif

