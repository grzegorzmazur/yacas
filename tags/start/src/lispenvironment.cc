
#include "choices.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispeval.h"
#include "lispatom.h"
#include "standard.h"
#include "lispuserfunc.h"
#include "mathuserfunc.h"


#define InternalEval iEvaluator->Eval

LispEnvironment::LispEnvironment(LispCommands& aCommands,
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
    : iPrecision(10),iEvalDepth(0),iMaxEvalDepth(1000),iSecure(0),
    iEvaluator(new BasicEvaluator),iLastUniqueId(1),
    iCommands(aCommands),
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
    iErrorOutput(iError),
    iLocalsList(NULL)
{
    iTrue=NULL;
    iFalse=NULL;

    iTrue = HashTable().LookUp("True");
    iFalse = HashTable().LookUp("False");
    iTrue->IncreaseRefCount();
    iFalse->IncreaseRefCount();
    iTrueAtom.Set(LispAtom::New(iTrue));
    iFalseAtom.Set(LispAtom::New(iFalse));
    PushLocalFrame(LispTrue);
}


LispEnvironment::~LispEnvironment()
{
    iTrue->DecreaseRefCount();
    iFalse->DecreaseRefCount();
    delete iEvaluator;
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
/*TODO remove?
    Check(iLocals.Get() != NULL,KLispErrInvalidStack);
    Check(iLocals.Get()->SubList() != NULL,KLispErrInvalidStack);

    LispIterator iter(*iLocals.Get()->SubList());
    while (iter() != NULL)
    {
        LispPtr* sub = iter()->SubList();
        Check(sub != NULL,KLispErrInvalidStack);
        if (sub->Get()->String() == aVariable)
            return iter.Ptr();

        iter.GoNext();
    }
    return NULL;
    */
}

void LispEnvironment::SetVariable(LispStringPtr aVariable, LispPtr& aValue)
{
    LispPtr *local = FindLocal(aVariable);
    if (local != NULL)
    {
        local->Set(aValue.Get());
        /*TODO remove?
         local->Get()->SubList()->Get()->Next().Set(aValue.Get());
         */
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
        /*TODO remove?
        aResult.Set(local->Get()->SubList()->Get()->Next().Get());
        */
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
/*TODO remove?
        local->Get()->SubList()->Get()->Next().Set(NULL);
        */
        return;
    }
    iGlobals.Release(aString);
}

void LispEnvironment::PushLocalFrame(LispBoolean aFenced)
{
    if (aFenced)
    {
        LocalVariableFrame *newFrame =
            new LocalVariableFrame(iLocalsList, NULL);
        iLocalsList = newFrame;
    }
    else
    {
        LocalVariableFrame *newFrame =
            new LocalVariableFrame(iLocalsList, iLocalsList->iFirst);
        iLocalsList = newFrame;
    }
    /*TODO remove?
    if (aFenced)
    {
        LispPtr newly;
        newly.Set(LispSubList::New(NULL));
        newly.Get()->Next().Set(iLocals.Get());
        iLocals.Set(newly.Get());
    }
    else
    {
        LISPASSERT(iLocals.Get()!=NULL);
        LISPASSERT(iLocals.Get()->SubList()!=NULL);
        LispPtr newly;
        newly.Set(LispSubList::New(iLocals.Get()->SubList()->Get()));
        newly.Get()->Next().Set(iLocals.Get());
        iLocals.Set(newly.Get());
    }
    */
}

void LispEnvironment::PopLocalFrame()
{
    LocalVariableFrame *nextFrame = iLocalsList->iNext;
    delete iLocalsList;
    iLocalsList = nextFrame;

/*TODO remove?
    LISPASSERT(iLocals.Get() != NULL);
    LispPtr next;
    next.Set(iLocals.Get()->Next().Get());
    iLocals.Set(next.Get());
    */
}

void LispEnvironment::NewLocal(LispStringPtr aVariable,LispObject* aValue)
{
    iLocalsList->Add(new LispLocalVariable(aVariable, aValue));
    /*TODO remove?
    LISPASSERT(iLocals.Get() != NULL);
    LISPASSERT(iLocals.Get()->SubList() != NULL);
    LispPtr newly;
    LispObject* newitem = LispSubList::New(LispAtom::New(aVariable));
    newitem->SubList()->Get()->Next().Set(aValue);
    newitem->Next().Set(iLocals.Get()->SubList()->Get());
    newly.Set(newitem);
    iLocals.Get()->SubList()->Set(newly.Get());
    */
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
    if (multiUserFunc != NULL)
    {
        LispInt arity = InternalListLength(aArguments)-1;
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

void LispEnvironment::TryRetract(LispStringPtr aOperator,LispInt aArity)
{
    LispMultiUserFunction* multiUserFunc = iUserFunctions.LookUp(aOperator);
    if (multiUserFunc)
    {
        multiUserFunc->DeleteBase(aArity);
    }
}

void LispEnvironment::DeclareRuleBase(LispStringPtr aOperator, LispPtr& aParameters)
{
    LispMultiUserFunction* multiUserFunc = MultiUserFunction(aOperator);
    // add an operator with this arity to the multiuserfunc.
    multiUserFunc->DefineRuleBase(new BranchingUserFunction(aParameters));


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



void LispEnvironment::SetCommand(LispEvalCaller aEvaluatorFunc, LispCharPtr aString)
{
#ifdef YACAS_DEBUG
    extern long theNrDefinedBuiltIn;
    theNrDefinedBuiltIn++;
#endif
    Commands().SetAssociation(LispEvaluator(aEvaluatorFunc),
                            HashTable().LookUp(aString,LispTrue));
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
        return "No Error";
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
        return "Not Enough Memory";
    case KInvalidToken:
        return "Empty token during parsing";
    case KLispErrInvalidExpression:
        return "Error parsing expression";
    case KLispErrUnprintableToken:
        return "Unprintable atom";
    case KLispErrFileNotFound:
        return "File Not Found";
    case KLispErrReadingFile:
        return "Error reading file";
    case KLispErrCreatingUserFunction:
        return "Could Not Create User Function";
    case KLispErrCreatingRule:
        return "Could Not Create Rule";
    case KLispErrArityAlreadyDefined:
        return "Rule Base With This Arity Already Defined";
    case KLispErrCommentToEndOfFile:
        return "Reaching end of file within a comment block";
    case KLispErrNotString:
        return "Argument Is Not A String";
    case KLispErrNotInteger:
        return "Argument Is Not A Integer";
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
    }
    return "Unspecified Error";
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


