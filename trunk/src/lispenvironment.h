/** \file lispenvironment.h
 *  General environment access.
 *
 */


#ifndef __lispenvironment_h__
#define __lispenvironment_h__

#include "lispobject.h"
#include "lisphash.h"
#include "lispevalhash.h"
#include "lispcleanupstack.h"
#include "deffile.h"
#include "ramdisk.h"
#include "lispio.h"
#include "stringio.h"
#include "lispglobals.h"
#include "lispplugin.h"


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
class LispEnvironment
{
public:
    LispEnvironment(LispCommands& aCommands,
                    LispUserFunctions& aUserFunctions,
                    LispGlobal& aGlobals,
                    LispHashTable& aHashTable,
                    LispOutput* aOutput,
                    LispPrinter& aPrinter,
                    LispOperators &aPreFixOperators,
                    LispOperators &aInFixOperators,
                    LispOperators &aPostFixOperators,
                    LispOperators &aBodiedOperators,
                    LispInput*    aCurrentInput);
    ~LispEnvironment();
public:
    void SetVariable(LispStringPtr aString, LispPtr& aValue);
    void GetVariable(LispStringPtr aVariable,LispPtr& aResult);
    void SetGlobalEvaluates(LispStringPtr aVariable);

    void UnsetVariable(LispStringPtr aString);
    void PushLocalFrame(LispBoolean aFenced);
    void PopLocalFrame();
    void NewLocal(LispStringPtr aVariable,LispObject* aValue);
public:
    inline LispCommands& Commands();
    void SetCommand(LispEvalCaller aEvaluatorFunc, LispCharPtr aString);

    inline  LispHashTable& HashTable();
    LispUserFunction* UserFunction(LispPtr& aArguments);
    LispUserFunction* UserFunction(LispStringPtr aName,LispInt aArity);
    LispMultiUserFunction* MultiUserFunction(LispStringPtr aArguments);
    LispDefFiles& DefFiles();
    void DeclareRuleBase(LispStringPtr aOperator, LispPtr& aParameters);
    void DefineRule(LispStringPtr aOperator,LispInt aArity,
                            LispInt aPrecedence, LispPtr& aPredicate,
                            LispPtr& aBody);
    void DefineRulePattern(LispStringPtr aOperator,LispInt aArity,
                            LispInt aPrecedence, LispPtr& aPredicate,
                            LispPtr& aBody);
    void UnFenceRule(LispStringPtr aOperator,LispInt aArity);
    void TryRetract(LispStringPtr aOperator,LispInt aArity);
    void HoldArgument(LispStringPtr  aOperator,LispStringPtr aVariable);

public:
    inline void SetPrecision(LispInt aPrecision);
    inline LispInt Precision(void);
public:
    inline void SetPrettyPrinter(LispStringPtr aPrettyPrinter);
    inline LispStringPtr PrettyPrinter(void);
public:
    LispInt GetUniqueId();
public:
    LispPrinter& CurrentPrinter();
public:
    LispOperators& PreFix();
    LispOperators& InFix();
    LispOperators& PostFix();
    LispOperators& Bodied();
public:
    LispInput* CurrentInput();
    void SetCurrentInput(LispInput* aInput);
public:
    LispOutput* CurrentOutput();
    void SetCurrentOutput(LispOutput* aOutput);
public:
    void SetUserError(LispCharPtr aErrorString);
    LispCharPtr ErrorString(LispInt aError);
    
protected:
    LispInt iPrecision;
public:
    InputDirectories iInputDirectories;
    DeletingLispCleanup iCleanup;
    LispInt iEvalDepth;
    LispInt iMaxEvalDepth;
    LispRamDisk iRamDisk;
    LispEvaluatorBase* iEvaluator;

public: // Error information when some error occurs.
    InputStatus iInputStatus;
    LispInt iSecure;
public: // pre-found
    LispStringPtr iTrue;
    LispStringPtr iFalse;
    LispPtr iTrueAtom;
    LispPtr iFalseAtom;
    LispInt iLastUniqueId;

public: // Error reporting
    LispString iError;
    StringOutput iErrorOutput;
    CDeletingArrayGrower<LispDllBase*> iDlls;

private:
    LispPtr *FindLocal(LispStringPtr aVariable);

private:

    class LispLocalVariable
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
    class LocalVariableFrame
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
    LispCommands&  iCommands;
    LispUserFunctions& iUserFunctions;
    LispHashTable& iHashTable;
    LispDefFiles   iDefFiles;
    LispPrinter&   iPrinter;
    LispOutput*    iCurrentOutput;

    LispGlobal&    iGlobals;

    LocalVariableFrame *iLocalsList;
//    LispPtr        iLocals;

    LispOperators& iPreFixOperators;
    LispOperators& iInFixOperators;
    LispOperators& iPostFixOperators;
    LispOperators& iBodiedOperators;

    LispInput* iCurrentInput;

    LispCharPtr theUserError;

    LispStringPtr iPrettyPrinter;
};


inline void LispEnvironment::SetPrecision(LispInt aPrecision)
{
    iPrecision = aPrecision;
}

inline LispInt LispEnvironment::Precision(void)
{
    return iPrecision;
}

inline LispCommands& LispEnvironment::Commands()
{
    return iCommands;
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


class LispLocalEvaluator
{
public:
    LispLocalEvaluator(LispEnvironment& aEnvironment,LispEvaluatorBase* aNewEvaluator);
    ~LispLocalEvaluator();
    
private:
    LispEvaluatorBase* iPreviousEvaluator;
    LispEnvironment& iEnvironment;
};

class LispLocalTrace
{
public:
    LispLocalTrace(LispUserFunction* aUserFunc);
    ~LispLocalTrace();
private:
    LispUserFunction* iUserFunc;
};

class LocalArgs
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

