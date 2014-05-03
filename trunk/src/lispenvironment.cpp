
#include "yacas/yacasbase.h"
#include "yacas/choices.h"
#include "yacas/lispenvironment.h"
#include "yacas/lispeval.h"
#include "yacas/lispatom.h"
#include "yacas/standard.h"
#include "yacas/lispuserfunc.h"
#include "yacas/mathuserfunc.h"
#include "yacas/errors.h"

// we need this only for digits_to_bits
#include "yacas/numbers.h"

#ifdef YACAS_DEBUG
#include <stdio.h> // Safe, only included if YACAS_DEBUG is defined
#endif

#define InternalEval iEvaluator->Eval


LispEnvironment::LispEnvironment(
                    YacasCoreCommands& aCoreCommands,
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
                    LispInt aStackSize)
    :
    iPrecision(10),  // default user precision of 10 decimal digits
    iBinaryPrecision(34),  // same as 34 bits
    iInputDirectories(),
    iCleanup(),
    iEvalDepth(0),
    iMaxEvalDepth(1000),
    stop_evaluation(false),
    iEvaluator(NEW BasicEvaluator),
    iInputStatus(),
    secure(false),
    iTrue(),
    iFalse(),
    iEndOfFile(),
    iEndStatement(),
    iProgOpen(),
    iProgClose(),
    iNth(),
    iBracketOpen(),
    iBracketClose(),
    iListOpen(),
    iListClose(),
    iComma(),
    iList(),
    iProg(),
    iLastUniqueId(1),
    iError(),
    iErrorOutput(iError),
    iDebugger(NULL),
    iLocalsList(NULL),
    iInitialOutput(aOutput),
    iCoreCommands(aCoreCommands),
    iUserFunctions(aUserFunctions),
    iHashTable(aHashTable),
    iDefFiles(),
    iPrinter(aPrinter),
    iCurrentOutput(aOutput),
    iGlobals(aGlobals),
    iPreFixOperators(aPreFixOperators),
    iInFixOperators(aInFixOperators),
    iPostFixOperators(aPostFixOperators),
    iBodiedOperators(aBodiedOperators),
    iCurrentInput(aCurrentInput),
    iPrettyReader(NULL),
    iPrettyPrinter(NULL),
    iDefaultTokenizer(),
    iCommonLispTokenizer(),
    iXmlTokenizer(),
    iCurrentTokenizer(&iDefaultTokenizer),
    iStack(aStackSize)
{
    iTrue = LispAtom::New(*this,"True");
    iFalse = LispAtom::New(*this,"False");

    iEndOfFile    = LispAtom::New(*this,"EndOfFile");
    iEndStatement = LispAtom::New(*this,";");
    iProgOpen     = LispAtom::New(*this,"[");
    iProgClose    = LispAtom::New(*this,"]");
    iNth          = LispAtom::New(*this,"Nth");
    iBracketOpen  = LispAtom::New(*this,"(");
    iBracketClose = LispAtom::New(*this,")");
    iListOpen     = LispAtom::New(*this,"{");
    iListClose    = LispAtom::New(*this,"}");
    iComma        = LispAtom::New(*this,",");
    iList         = LispAtom::New(*this,"List");
    iProg         = LispAtom::New(*this,"Prog");
    PushLocalFrame(true);
}


LispEnvironment::~LispEnvironment()
{
  PopLocalFrame();

  assert(!iLocalsList);
  delete iEvaluator;
  if (iDebugger) delete iDebugger;
}

void LispEnvironment::SetPrecision(LispInt aPrecision)
{
    iPrecision = aPrecision;  // precision in decimal digits
  iBinaryPrecision = digits_to_bits(aPrecision, BASE10);  // in bits
}

LispInt LispEnvironment::GetUniqueId()
{
    return iLastUniqueId++;
}


LispPtr *LispEnvironment::FindLocal(LispString * aVariable)
{
    if (!iLocalsList)
        throw LispErrInvalidStack();

    LispLocalVariable *t = iLocalsList->iFirst;

    while (t)
    {
        if (t->iVariable == aVariable)
        {
            return &t->iValue;
        }
        t = t->iNext;
    }
    return NULL;
}


#ifdef YACAS_DEBUG
void LispEnvironment::DebugModeVerifySettingGlobalVariables(LispPtr & aVariable, bool aGlobalLazyVariable)
{
  LispString *varString = aVariable->String();
  LispPtr *local = FindLocal(varString);
  if (local)
  {
    if (aGlobalLazyVariable)
    {
      printf("WARNING: setting local variable \"%s\" (file %s, line %d), but doing it through a method that is trying to set a global lazy variable. This is probably unintended.\n",
        varString->c_str(),
        aVariable->iFileName,
        aVariable->iLine);
    }
    return;
  }

  {
    int warn = 1;
    // If a variable is guarded with LocalSymbol it can not interfere with other scripts.
    if ((*varString)[0] ==  '$') warn = 0;
    if (aGlobalLazyVariable) warn = 0;
    if (warn)
      printf("WARNING: setting global variable \"%s\" (file %s, line %d) (global variables might have undesired side effects, please use Local or LocalSymbols).\n",
        varString->c_str(),
        aVariable->iFileName,
        aVariable->iLine);
  }
}
#endif // YACAS_DEBUG

void LispEnvironment::SetVariable(LispString * aVariable, LispPtr& aValue, bool aGlobalLazyVariable)
{
  LispPtr *local = FindLocal(aVariable);
  if (local)
  {
    (*local) = (aValue);
    return;
  }

  iGlobals.SetAssociation(LispGlobalVariable(aValue), aVariable);
  if (aGlobalLazyVariable)
  {
    //TODO we just added the variable! We should not need to re-look it up! Optimize!
    LispGlobalVariable *l = iGlobals.LookUp(aVariable);
    assert(l);
    l->SetEvalBeforeReturn(true);
  }
}

void LispEnvironment::GetVariable(LispString * aVariable,LispPtr& aResult)
{
  aResult = (NULL);
  LispPtr *local = FindLocal(aVariable);
  if (local)
  {
    aResult = ((*local));
    return;
  }
  LispGlobalVariable *l = iGlobals.LookUp(aVariable);
  if (l)
  {
    if (l->iEvalBeforeReturn)
    {
      InternalEval(*this, aResult, l->iValue);
      // re-lookup the global variable, as this pointer might now be invalid due to the evaluation actually changing the global itself.
      l = iGlobals.LookUp(aVariable);

      l->iValue = (aResult);
      l->iEvalBeforeReturn = false;
      return;
    }
    else
    {
      aResult = (l->iValue);
      return;
    }
  }
}

void LispEnvironment::UnsetVariable(LispString * aString)
{
    LispPtr *local = FindLocal(aString);
    if (local)
    {
        (*local) = (NULL);
        return;
    }
    iGlobals.Release(aString);
}

void LispEnvironment::PushLocalFrame(bool aFenced)
{
    if (aFenced)
    {
        LocalVariableFrame *newFrame =
            NEW LocalVariableFrame(iLocalsList, NULL);
        iLocalsList = newFrame;
    }
    else
    {
        LocalVariableFrame *newFrame =
            NEW LocalVariableFrame(iLocalsList, iLocalsList->iFirst);
        iLocalsList = newFrame;
    }
}

void LispEnvironment::PopLocalFrame()
{
    assert(iLocalsList);
    LocalVariableFrame *nextFrame = iLocalsList->iNext;
    delete iLocalsList;
    iLocalsList = nextFrame;
}

void LispEnvironment::NewLocal(LispString * aVariable,LispObject* aValue)
{
    assert(iLocalsList);
    iLocalsList->Add(NEW LispLocalVariable(aVariable, aValue));
}

void LispEnvironment::CurrentLocals(LispPtr& aResult)
{
  LispEnvironment::LocalVariableFrame* fr = iLocalsList;
  LispEnvironment::LispLocalVariable* ptr = fr->iFirst;

  LispEnvironment& aEnvironment = *this; //Pity, but we need this for the macros to work
  LispObject* locals = NULL;
  while (ptr)
  {
    locals = LA(ATOML(ptr->iVariable->c_str()))+LA(locals);
//    printf("%s ",ptr->iVariable->c_str());
    ptr = ptr->iNext;
  }
  aResult = (LIST(LA(ATOML("List")) + LA(locals)));
}



LispPrinter& LispEnvironment::CurrentPrinter()
{
    return iPrinter;
}

LispDefFiles& LispEnvironment::DefFiles()
{
    return iDefFiles;
}

LispOperators& LispEnvironment::PreFix()
{
    return iPreFixOperators;
}
LispOperators& LispEnvironment::InFix()
{
    return iInFixOperators;
}
LispOperators& LispEnvironment::PostFix()
{
    return iPostFixOperators;
}
LispOperators& LispEnvironment::Bodied()
{
    return iBodiedOperators;
}


LispInput* LispEnvironment::CurrentInput()
{
    return iCurrentInput;
}

void LispEnvironment::SetCurrentInput(LispInput* aInput)
{
    iCurrentInput = aInput;
}

LispOutput* LispEnvironment::CurrentOutput()
{
    return iCurrentOutput;
}

void LispEnvironment::SetCurrentOutput(LispOutput* aOutput)
{
    iCurrentOutput = aOutput;
}



LispUserFunction* LispEnvironment::UserFunction(LispPtr& aArguments)
{
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aArguments->String());
    if (multiUserFunc)
    {
        LispInt arity = InternalListLength(aArguments)-1;
        CHECKPTR(multiUserFunc->UserFunc(arity));
        return  multiUserFunc->UserFunc(arity);
    }
    return NULL;
}


LispUserFunction* LispEnvironment::UserFunction(LispString * aName,LispInt aArity)
{
    LispMultiUserFunction* multiUserFunc = iUserFunctions.LookUp(aName);
    if (multiUserFunc)
    {
        return  multiUserFunc->UserFunc(aArity);
    }
    return NULL;
}



void LispEnvironment::UnFenceRule(LispString * aOperator,LispInt aArity)
{
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aOperator);

    if (!multiUserFunc)
        throw LispErrInvalidArg();

    LispUserFunction* userFunc = multiUserFunc->UserFunc(aArity);

    if (!userFunc)
        throw LispErrInvalidArg();

    userFunc->UnFence();
}

void LispEnvironment::Retract(LispString * aOperator,LispInt aArity)
{
    LispMultiUserFunction* multiUserFunc = iUserFunctions.LookUp(aOperator);
    if (multiUserFunc)
    {
        multiUserFunc->DeleteBase(aArity);
    }
}

void LispEnvironment::DeclareRuleBase(LispString * aOperator,
                                      LispPtr& aParameters,
                                      LispInt aListed)
{
    LispMultiUserFunction* multiUserFunc = MultiUserFunction(aOperator);

    /*
     if (multiUserFunc->iFileToOpen)
    {
        LISPASSERT(multiUserFunc->iFileToOpen->iIsLoaded);
        }
        */

    // add an operator with this arity to the multiuserfunc.
    BranchingUserFunction *newFunc;
    if (aListed)
    {
        newFunc = NEW ListedBranchingUserFunction(aParameters);
    }
    else
    {
        newFunc = NEW BranchingUserFunction(aParameters);
    }
    multiUserFunc->DefineRuleBase(newFunc);

  DBG_({ extern long theNrDefinedUser; theNrDefinedUser++; })
}

void LispEnvironment::DeclareMacroRuleBase(LispString * aOperator, LispPtr& aParameters, LispInt aListed)
{
    LispMultiUserFunction* multiUserFunc = MultiUserFunction(aOperator);
    MacroUserFunction *newFunc;
    if (aListed)
    {
      newFunc = NEW ListedMacroUserFunction(aParameters);
    }
    else
    {
      newFunc = NEW MacroUserFunction(aParameters);
    }
    multiUserFunc->DefineRuleBase(newFunc);

  DBG_({ extern long theNrDefinedUser; theNrDefinedUser++; })
}




LispMultiUserFunction* LispEnvironment::MultiUserFunction(LispString * aOperator)
{
    // Find existing multiuser func.
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aOperator);

    // If none exists, add one to the user functions list
    if (!multiUserFunc)
    {
        LispMultiUserFunction newMulti;
        iUserFunctions.SetAssociation(newMulti, aOperator);
        multiUserFunc =
            iUserFunctions.LookUp(aOperator);

        if (!multiUserFunc)
            throw LispErrCreatingUserFunction();
    }
    return multiUserFunc;
}






void LispEnvironment::HoldArgument(LispString *  aOperator, LispString * aVariable)
{
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aOperator);

    if (!multiUserFunc)
        throw LispErrInvalidArg();

    multiUserFunc->HoldArgument(aVariable);
}


void LispEnvironment::DefineRule(LispString * aOperator,LispInt aArity,
                                 LispInt aPrecedence, LispPtr& aPredicate,
                                 LispPtr& aBody)
{
    // Find existing multiuser func.
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aOperator);

    if (!multiUserFunc)
        throw LispErrCreatingRule();

    // Get the specific user function with the right arity
    LispUserFunction* userFunc = multiUserFunc->UserFunc(aArity);

    if (!userFunc)
        throw LispErrCreatingRule();

    // Declare a new evaluation rule


    if (IsTrue(*this, aPredicate))
    {
//        printf("FastPredicate on %s\n",aOperator->String());
        userFunc->DeclareRule(aPrecedence, aBody);
    }
    else
        userFunc->DeclareRule(aPrecedence, aPredicate,aBody);
}

void LispEnvironment::DefineRulePattern(LispString * aOperator,LispInt aArity,
                                        LispInt aPrecedence, LispPtr& aPredicate,
                                        LispPtr& aBody)
{
    // Find existing multiuser func.
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aOperator);

    if (!multiUserFunc)
        throw LispErrCreatingRule();

    // Get the specific user function with the right arity
    LispUserFunction* userFunc = multiUserFunc->UserFunc(aArity);

    if (!userFunc)
        throw LispErrCreatingRule();

    // Declare a new evaluation rule
    userFunc->DeclarePattern(aPrecedence, aPredicate,aBody);
}

void LispEnvironment::SetCommand(YacasEvalCaller aEvaluatorFunc, const LispChar * aString,LispInt aNrArgs,LispInt aFlags)
{
  DBG_({ extern long theNrDefinedBuiltIn; theNrDefinedBuiltIn++; })
  YacasEvaluator eval(aEvaluatorFunc,aNrArgs,aFlags);
  CoreCommands().SetAssociation(eval,HashTable().LookUp(aString));
}

void LispEnvironment::RemoveCoreCommand(LispChar * aString)
{
  CoreCommands().Release(HashTable().LookUp(aString));
}

LispString * LispEnvironment::FindCachedFile(const LispChar * aFileName)
{
  return NULL;
}




void LispLocalFrame::Delete()
{
    iEnvironment.PopLocalFrame();
}

void LispSecureFrame::Delete()
{
    iEnvironment.secure = previous_secure;
}

void LispLocalInput::Delete()
{
    iEnvironment.SetCurrentInput(iPreviousInput);
}

void LispLocalOutput::Delete()
{
    iEnvironment.SetCurrentOutput(iPreviousOutput);
}


LispLocalEvaluator::LispLocalEvaluator(LispEnvironment& aEnvironment,LispEvaluatorBase* aNewEvaluator)
  : iPreviousEvaluator(aEnvironment.iEvaluator),iEnvironment(aEnvironment)
{
  aEnvironment.iEvaluator = aNewEvaluator;
}
LispLocalEvaluator::~LispLocalEvaluator()
{
    delete iEnvironment.iEvaluator;
    iEnvironment.iEvaluator = iPreviousEvaluator;
}

LispLocalTrace::LispLocalTrace(LispUserFunction* aUserFunc) : iUserFunc(aUserFunc)
{
  if (iUserFunc!=NULL)
    iUserFunc->Trace();
}
LispLocalTrace::~LispLocalTrace()
{
  if (iUserFunc!=NULL)
    iUserFunc->UnTrace();
}


