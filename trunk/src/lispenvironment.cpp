
#include "yacasbase.h"
#include "choices.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispeval.h"
#include "lispatom.h"
#include "standard.h"
#include "lispuserfunc.h"
#include "mathuserfunc.h"
#include "errors.h"

// we need this only for digits_to_bits
#include "numbers.h"

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
                    LispInput*    aCurrentInput)
    : 
    iPrecision(10),	// default user precision of 10 decimal digits
	iBinaryPrecision(34),	// same as 34 bits
    iEvalDepth(0),
    iMaxEvalDepth(1000),
    iArchive(NULL),
    iEvaluator(NEW BasicEvaluator),
    iSecure(0),
    iLastUniqueId(1),
    iErrorOutput(iError),
    iDebugger(NULL),
    iLocalsList(NULL),
    iInitialOutput(aOutput),
    iCoreCommands(aCoreCommands),
    iUserFunctions(aUserFunctions),
    iHashTable(aHashTable),
    iPrinter(aPrinter),
    iCurrentOutput(aOutput),
    iGlobals(aGlobals),
    iPreFixOperators(aPreFixOperators),
    iInFixOperators(aInFixOperators),
    iPostFixOperators(aPostFixOperators),
    iBodiedOperators(aBodiedOperators),
    iCurrentInput(aCurrentInput),
    theUserError(NULL),
    iPrettyPrinter(NULL),
    iCurrentTokenizer(&iDefaultTokenizer)
{
    iTrue=NULL;
    iFalse=NULL;

    iTrue = HashTable().LookUp("True");
    iFalse = HashTable().LookUp("False");

    iEndOfFile    = iHashTable.LookUp("EndOfFile");
    iEndStatement = iHashTable.LookUp(";");
    iProgOpen     = iHashTable.LookUp("[");
    iProgClose    = iHashTable.LookUp("]");
    iNth          = iHashTable.LookUp("Nth");
    iBracketOpen  = iHashTable.LookUp("(");
    iBracketClose = iHashTable.LookUp(")");
    iListOpen     = iHashTable.LookUp("{");
    iListClose    = iHashTable.LookUp("}");
    iComma        = iHashTable.LookUp(",");
    iList         = iHashTable.LookUp("List");
    iProg         = iHashTable.LookUp("Prog");

    iEndOfFile   ->IncreaseRefCount();
    iEndStatement->IncreaseRefCount();
    iProgOpen    ->IncreaseRefCount();
    iProgClose   ->IncreaseRefCount();
    iNth         ->IncreaseRefCount();
    iBracketOpen ->IncreaseRefCount();
    iBracketClose->IncreaseRefCount();
    iListOpen    ->IncreaseRefCount();
    iListClose   ->IncreaseRefCount();
    iComma       ->IncreaseRefCount();
    iList        ->IncreaseRefCount();
    iProg        ->IncreaseRefCount();

    iTrue        ->IncreaseRefCount();
    iFalse       ->IncreaseRefCount();

    iTrueAtom.Set(LispAtom::New(*this,iTrue));
    iFalseAtom.Set(LispAtom::New(*this,iFalse));
    PushLocalFrame(LispTrue);
    iCTokenizer.SetRemarkReceiver(*this);
}


LispEnvironment::~LispEnvironment()
{
    PopLocalFrame();

    {
        LispInt i,nr=iDlls.NrItems();
        for (i=0;i<nr;i++)
        {
            iDlls[i]->Close(*this);
            delete iDlls[i];
            iDlls[i] = NULL;
        }
    }
    
    LISPASSERT(iLocalsList == NULL);
    iTrue        ->DecreaseRefCount();
    iFalse       ->DecreaseRefCount();

    iEndOfFile   ->DecreaseRefCount();
    iEndStatement->DecreaseRefCount();
    iProgOpen    ->DecreaseRefCount();
    iProgClose   ->DecreaseRefCount();
    iNth         ->DecreaseRefCount();
    iBracketOpen ->DecreaseRefCount();
    iBracketClose->DecreaseRefCount();
    iListOpen    ->DecreaseRefCount();
    iListClose   ->DecreaseRefCount();
    iComma       ->DecreaseRefCount();
    iList        ->DecreaseRefCount();
    iProg        ->DecreaseRefCount();

    delete iEvaluator;
    if (iDebugger) delete iDebugger;
    delete iArchive;
}

void LispEnvironment::SetPrecision(LispInt aPrecision)
{
    iPrecision = aPrecision;	// precision in decimal digits
	iBinaryPrecision = digits_to_bits(aPrecision, BASE10);	// in bits
}

LispInt LispEnvironment::GetUniqueId()
{
    return iLastUniqueId++;
}


LispPtr *LispEnvironment::FindLocal(LispStringPtr aVariable)
{
    Check(iLocalsList != NULL,KLispErrInvalidStack);
//    Check(iLocalsList->iFirst != NULL,KLispErrInvalidStack);
    LispLocalVariable *t = iLocalsList->iFirst;

    while (t != NULL)
    {
        if (t->iVariable == aVariable)
        {
            return &t->iValue;
        }
        t = t->iNext;
    }
    return NULL;
}

void LispEnvironment::SetVariable(LispStringPtr aVariable, LispPtr& aValue)
{
    LispPtr *local = FindLocal(aVariable);
    if (local != NULL)
    {
        local->Set(aValue.Get());
        return;
    }

    iGlobals.SetAssociation(LispGlobalVariable(aValue), aVariable);
}

void LispEnvironment::GetVariable(LispStringPtr aVariable,LispPtr& aResult)
{
    aResult.Set(NULL);
    LispPtr *local = FindLocal(aVariable);
    if (local != NULL)
    {
        aResult.Set(local->Get());
        return;
    }
    LispGlobalVariable *l = iGlobals.LookUp(aVariable);
    if (l)
    {
        if (l->iEvalBeforeReturn)
        {
            InternalEval(*this, aResult, l->iValue);
            l->iValue.Set(aResult.Get());
            l->iEvalBeforeReturn = LispFalse;
            return;
        }
        else
        {
            aResult.Set(l->iValue.Get());
            return;
        }
    }
}

void LispEnvironment::SetGlobalEvaluates(LispStringPtr aVariable)
{
    LispGlobalVariable *l = iGlobals.LookUp(aVariable);
    Check(l != NULL,KLispErrInvalidArg);
    l->SetEvalBeforeReturn(LispTrue);
}

void LispEnvironment::UnsetVariable(LispStringPtr aString)
{
    LispPtr *local = FindLocal(aString);
    if (local != NULL)
    {
        local->Set(NULL);
        return;
    }
    iGlobals.Release(aString);
}

void LispEnvironment::PushLocalFrame(LispBoolean aFenced)
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
    LISPASSERT(iLocalsList != NULL);
    LocalVariableFrame *nextFrame = iLocalsList->iNext;
    delete iLocalsList;
    iLocalsList = nextFrame;
}

void LispEnvironment::NewLocal(LispStringPtr aVariable,LispObject* aValue)
{
    LISPASSERT(iLocalsList != NULL);
    iLocalsList->Add(NEW LispLocalVariable(aVariable, aValue));
}

void LispEnvironment::CurrentLocals(LispPtr& aResult)
{
  LispEnvironment::LocalVariableFrame* fr = iLocalsList;
  LispEnvironment::LispLocalVariable* ptr = fr->iFirst;

  LispEnvironment& aEnvironment = *this; //Pity, but we need this for the macros to work
  LispObject* locals = NULL;
  while (ptr != NULL)
  {
    locals = LA(ATOML(ptr->iVariable->String()))+LA(locals);
//    printf("%s ",ptr->iVariable->String());
    ptr = ptr->iNext;
  }
  aResult.Set(LIST(LA(ATOML("List")) + LA(locals)));  
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
        iUserFunctions.LookUp(aArguments.Get()->String());
//    CHECKPTR(multiUserFunc);
    if (multiUserFunc != NULL)
    {
        LispInt arity = InternalListLength(aArguments)-1;
        CHECKPTR(multiUserFunc->UserFunc(arity));
        return  multiUserFunc->UserFunc(arity);
    }
    return NULL;
}


LispUserFunction* LispEnvironment::UserFunction(LispStringPtr aName,LispInt aArity)
{
    LispMultiUserFunction* multiUserFunc = iUserFunctions.LookUp(aName);
    if (multiUserFunc != NULL)
    {
        return  multiUserFunc->UserFunc(aArity);
    }
    return NULL;
}



void LispEnvironment::UnFenceRule(LispStringPtr aOperator,LispInt aArity)
{
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aOperator);

    Check(multiUserFunc != NULL, KLispErrInvalidArg);
    LispUserFunction* userFunc = multiUserFunc->UserFunc(aArity);
    Check(userFunc != NULL, KLispErrInvalidArg);
    userFunc->UnFence();
}

void LispEnvironment::Retract(LispStringPtr aOperator,LispInt aArity)
{
    LispMultiUserFunction* multiUserFunc = iUserFunctions.LookUp(aOperator);
    if (multiUserFunc)
    {
        multiUserFunc->DeleteBase(aArity);
    }
}

void LispEnvironment::DeclareRuleBase(LispStringPtr aOperator,
                                      LispPtr& aParameters,
                                      LispInt aListed)
{
    LispMultiUserFunction* multiUserFunc = MultiUserFunction(aOperator);

    /*
     if (multiUserFunc->iFileToOpen != NULL)
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

#ifdef YACAS_DEBUG
    {
        extern long theNrDefinedUser;
        theNrDefinedUser++;
    }
#endif
}

void LispEnvironment::DeclareMacroRuleBase(LispStringPtr aOperator, LispPtr& aParameters, LispInt aListed)
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

#ifdef YACAS_DEBUG
    {
        extern long theNrDefinedUser;
        theNrDefinedUser++;
    }
#endif
}




LispMultiUserFunction* LispEnvironment::MultiUserFunction(LispStringPtr aOperator)
{
    // Find existing multiuser func.
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aOperator);

    // If none exists, add one to the user functions list
    if (multiUserFunc == NULL)
    {
        LispMultiUserFunction newMulti;
        iUserFunctions.SetAssociation(newMulti, aOperator);
        multiUserFunc =
            iUserFunctions.LookUp(aOperator);
        Check(multiUserFunc != NULL, KLispErrCreatingUserFunction);
    }
    return multiUserFunc;
}






void LispEnvironment::HoldArgument(LispStringPtr  aOperator, LispStringPtr aVariable)
{
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aOperator);
    Check(multiUserFunc != NULL,KLispErrInvalidArg);
    multiUserFunc->HoldArgument(aVariable);
}


void LispEnvironment::DefineRule(LispStringPtr aOperator,LispInt aArity,
                                 LispInt aPrecedence, LispPtr& aPredicate,
                                 LispPtr& aBody)
{
    // Find existing multiuser func.
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aOperator);
    Check(multiUserFunc != NULL, KLispErrCreatingRule);

    // Get the specific user function with the right arity
    LispUserFunction* userFunc = multiUserFunc->UserFunc(aArity);
    Check(userFunc != NULL, KLispErrCreatingRule);
    
    // Declare a new evaluation rule
    

    if (IsTrue(*this, aPredicate))
    {
//        printf("FastPredicate on %s\n",aOperator->String());
        userFunc->DeclareRule(aPrecedence, aBody);
    }
    else
        userFunc->DeclareRule(aPrecedence, aPredicate,aBody);
}

void LispEnvironment::DefineRulePattern(LispStringPtr aOperator,LispInt aArity,
                                        LispInt aPrecedence, LispPtr& aPredicate,
                                        LispPtr& aBody)
{
    // Find existing multiuser func.
    LispMultiUserFunction* multiUserFunc =
        iUserFunctions.LookUp(aOperator);
    Check(multiUserFunc != NULL, KLispErrCreatingRule);

    // Get the specific user function with the right arity
    LispUserFunction* userFunc = multiUserFunc->UserFunc(aArity);
    Check(userFunc != NULL, KLispErrCreatingRule);
    
    // Declare a new evaluation rule
    userFunc->DeclarePattern(aPrecedence, aPredicate,aBody);
}



void LispEnvironment::SetCommand(YacasEvalCaller aEvaluatorFunc, LispCharPtr aString,LispInt aNrArgs,LispInt aFlags)
{
#ifdef YACAS_DEBUG
    extern long theNrDefinedBuiltIn;
    theNrDefinedBuiltIn++;
#endif
    YacasEvaluator eval(aEvaluatorFunc,aNrArgs,aFlags);
    CoreCommands().SetAssociation(eval,HashTable().LookUp(aString,LispTrue));
}

void LispEnvironment::RemoveCoreCommand(LispCharPtr aString)
{
    CoreCommands().Release(HashTable().LookUp(aString,LispTrue));
}

void LispEnvironment::SetUserError(LispCharPtr aErrorString)
{
    theUserError=aErrorString;
}

LispCharPtr LispEnvironment::ErrorString(LispInt aError)
{
    LISPASSERT(aError>=0 && aError < KLispNrErrors);
    switch (aError)
    {
    case KLispErrNone:
        return "No error";
    case KLispErrInvalidArg:
        return "Invalid argument";
    case KLispErrWrongNumberOfArgs:
        return "Wrong number of arguments";
    case KLispErrNotList:
        return "Argument is not a list";
    case KLispErrListNotLongEnough:
        return "List not long enough";
    case KLispErrInvalidStack:
        return "Invalid stack";
    case KQuitting:
        return "Quitting...";
    case KLispErrNotEnoughMemory:
        return "Not enough memory";
    case KInvalidToken:
        return "Empty token during parsing";
    case KLispErrInvalidExpression:
        return "Error parsing expression";
    case KLispErrUnprintableToken:
        return "Unprintable atom";
    case KLispErrFileNotFound:
        return "File not found";
    case KLispErrReadingFile:
        return "Error reading file";
    case KLispErrCreatingUserFunction:
        return "Could not create user function";
    case KLispErrCreatingRule:
        return "Could not create rule";
    case KLispErrArityAlreadyDefined:
        return "Rule base with this arity already defined";
    case KLispErrCommentToEndOfFile:
        return "Reaching end of file within a comment block";
    case KLispErrNotString:
        return "Argument is not a string";
    case KLispErrNotInteger:
        return "Argument is not an integer";
    case KLispErrParsingInput:
        return "Error while parsing input";
    case KLispErrMaxRecurseDepthReached:
        return "Max evaluation stack depth reached.\nPlease use MaxEvalDepth to increase the stack size as needed.";
    case KLispErrDefFileAlreadyChosen:
        return "DefFile already chosen for function";
    case KLispErrDivideByZero:
        return "Divide by zero";
    case KLispErrNotAnInFixOperator:
        return "Trying to make a non-infix operator right-associative";
    case KLispErrIsNotInFix:
        return "Trying to get precedence of non-infix operator";
    case KLispErrSecurityBreach:
        return "Trying to perform an insecure action";
    case KLispErrLibraryNotFound:
        return "Could not find library";
    case KLispErrUserInterrupt:
        return "User interrupted calculation";
    case KLispErrUser:
        if (theUserError != NULL) //TODO should always be true!
            return theUserError;
        break;
    case KLispErrNonBooleanPredicateInPattern:
        return "Predicate doesn't evaluate to a boolean in pattern";
     case KLispErrGenericFormat: return GenericErrorBuf();
    }
    return "Unspecified Error";
}





LispStringPtr LispEnvironment::FindCachedFile(LispCharPtr aFileName)
{
    if (iArchive)
    {
        LispInt index = iArchive->iFiles.FindFile(aFileName);
        if (index>=0)
        {
            LispCharPtr contents = iArchive->iFiles.Contents(index);
            if (contents != NULL)
            {
                return NEW LispString(contents,LispFalse);
            }
//printf("File %s unsuccessfully decompressed\n",aFileName);
        }
    }

    LispStringPtr hashedname = HashTable().LookUp(aFileName);
    LispRamFile* ramFile=iRamDisk.LookUp(hashedname);
    if (ramFile != NULL)
    {
        return NEW LispString(ramFile->Contents()->String(),LispTrue);
    }
    return NULL;
}




void LispLocalFrame::Delete()
{
    iEnvironment.PopLocalFrame();
}

void LispSecureFrame::Delete()
{
    iEnvironment.iSecure = iPreviousSecure;
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
: iEnvironment(aEnvironment)
{
    iPreviousEvaluator = aEnvironment.iEvaluator;
    aEnvironment.iEvaluator = aNewEvaluator;
}
LispLocalEvaluator::~LispLocalEvaluator()
{
    delete iEnvironment.iEvaluator;
    iEnvironment.iEvaluator = iPreviousEvaluator;
}

LispLocalTrace::LispLocalTrace(LispUserFunction* aUserFunc)
{
    iUserFunc = aUserFunc;
    if (iUserFunc!=NULL)
        iUserFunc->Trace();
}
LispLocalTrace::~LispLocalTrace()
{
    if (iUserFunc!=NULL)
        iUserFunc->UnTrace();
}



/*
void LispErrorNrArgs::PrintError(LispOutput& aOutput)
{
    LispChar str[20];
    aOutput.Write("Expected ");
    InternalIntToAscii(str,iNrArgsExpected);
    aOutput.Write(str);
    aOutput.Write(" arguments, got ");
    InternalIntToAscii(str,iNrArgsReceived);
    aOutput.Write(str);
    aOutput.Write("\n");
}
*/

void CDllArray::DeleteNamed(LispCharPtr aName, LispEnvironment& aEnvironment)
{
    LispInt i,nr=NrItems();
    for (i=0;i<nr;i++)
    {
//printf("DLL: %s ",(*this)[i]->DllFileName());
        if (StrEqual(aName,(*this)[i]->DllFileName()))
        {
            (*this)[i]->Close(aEnvironment);
            delete (*this)[i];
            (*this)[i] = NULL;
            Delete(i,1);
//printf("deleted!\n");
            return;
        }
//printf("\n");
    }
}










