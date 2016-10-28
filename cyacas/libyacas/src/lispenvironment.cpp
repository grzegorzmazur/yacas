
#include "yacas/lispenvironment.h"
#include "yacas/lispeval.h"
#include "yacas/lispatom.h"
#include "yacas/standard.h"
#include "yacas/lispuserfunc.h"
#include "yacas/mathuserfunc.h"
#include "yacas/errors.h"

// we need this only for digits_to_bits
#include "yacas/numbers.h"

LispEnvironment::LispEnvironment(
                    YacasCoreCommands& aCoreCommands,
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
                    LispInput*    aCurrentInput)
    :
    iPrecision(10),  // default user precision of 10 decimal digits
    iBinaryPrecision(34),  // same as 34 bits
    iInputDirectories(),
    //iCleanup(),
    iEvalDepth(0),
    iMaxEvalDepth(1000),
    stop_evaluation(false),
    iEvaluator(new BasicEvaluator),
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
    iDebugger(nullptr),
    iInitialOutput(&aOutput),
    iCoreCommands(aCoreCommands),
    iUserFunctions(aUserFunctions),
    iHashTable(aHashTable),
    iDefFiles(),
    iPrinter(aPrinter),
    iCurrentOutput(&aOutput),
    iGlobals(aGlobals),
    iPreFixOperators(aPreFixOperators),
    iInFixOperators(aInFixOperators),
    iPostFixOperators(aPostFixOperators),
    iBodiedOperators(aBodiedOperators),
    protected_symbols(protected_symbols),
    iCurrentInput(aCurrentInput),
    iPrettyReader(nullptr),
    iPrettyPrinter(nullptr),
    iDefaultTokenizer(),
    iXmlTokenizer(),
    iCurrentTokenizer(&iDefaultTokenizer)
{
    iTrue = LispAtom::New(*this,"True");
    iFalse = LispAtom::New(*this,"False");

    Protect(iTrue->String());
    Protect(iFalse->String());

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

    Protect(iList->String());
    Protect(iProg->String());

    Protect(iHashTable.LookUp("Infinity"));
    Protect(iHashTable.LookUp("Undefined"));

    PushLocalFrame(true);
}


LispEnvironment::~LispEnvironment()
{
    delete iEvaluator;
    delete iDebugger;
}

void LispEnvironment::SetPrecision(int aPrecision)
{
    iPrecision = aPrecision;  // precision in decimal digits
  iBinaryPrecision = digits_to_bits(aPrecision, BASE10);  // in bits
}

int LispEnvironment::GetUniqueId()
{
    return iLastUniqueId++;
}


LispPtr* LispEnvironment::FindLocal(const LispString* aVariable)
{
    assert(!_local_frames.empty());

    std::size_t last = _local_vars.size();

    for (std::vector<LocalVariableFrame>::const_reverse_iterator f = _local_frames.rbegin(); f != _local_frames.rend(); ++f) {
        const std::size_t first = f->first;
        for (std::size_t i = last; i > first; --i)
            if (_local_vars[i - 1].var == aVariable)
                return &_local_vars[i - 1].val;

        if (f->fenced)
            break;

        last = first;
    }
    return nullptr;
}

void LispEnvironment::SetVariable(const LispString* aVariable, LispPtr& aValue, bool aGlobalLazyVariable)
{
    if (LispPtr *local = FindLocal(aVariable)) {
        *local = aValue;
        return;
    }

    // FIXME: or should local variables be protected as well?
    if (Protected(aVariable))
        throw LispErrProtectedSymbol(*aVariable);

    auto i = iGlobals.find(aVariable);
    if (i != iGlobals.end())
        i->second = LispGlobalVariable(aValue);
    else
        i = iGlobals.insert(std::make_pair(aVariable, LispGlobalVariable(aValue))).first;

    if (aGlobalLazyVariable)
        i->second.SetEvalBeforeReturn(true);
}

void LispEnvironment::GetVariable(const LispString* aVariable, LispPtr& aResult)
{
    aResult = nullptr;

    if (LispPtr* local = FindLocal(aVariable)) {
        aResult = *local;
        return;
    }

    auto i = iGlobals.find(aVariable);

    if (i != iGlobals.end()) {
        LispGlobalVariable* l = &i->second;
        if (l->iEvalBeforeReturn) {
            iEvaluator->Eval(*this, aResult, l->iValue);
            // re-lookup the global variable, as this pointer might now be invalid due to the evaluation actually changing the global itself.
            l = &iGlobals.find(aVariable)->second;

            l->iValue = aResult;
            l->iEvalBeforeReturn = false;
        } else {
            aResult = l->iValue;
        }
    }
}

void LispEnvironment::UnsetVariable(const LispString* var)
{
    if (LispPtr* local = FindLocal(var))
        *local = nullptr;
    else {
        // FIXME: or should local variables be protected as well?
        if (Protected(var))
            throw LispErrProtectedSymbol(*var);
        iGlobals.erase(var);
    }
}

void LispEnvironment::PushLocalFrame(bool fenced)
{
    _local_frames.emplace_back(_local_vars.size(), fenced);
}

void LispEnvironment::PopLocalFrame()
{
    assert(!_local_frames.empty());

    _local_vars.erase(_local_vars.begin() + _local_frames.back().first, _local_vars.end());
    _local_frames.pop_back();
}

void LispEnvironment::NewLocal(const LispString* var, LispObject* val)
{
    assert(!_local_frames.empty());

    _local_vars.emplace_back(var, val);
}

void LispEnvironment::CurrentLocals(LispPtr& aResult)
{
    assert(!_local_frames.empty());

    LispObject* locals = nullptr;

    std::size_t last = _local_vars.size();

    for (std::vector<LocalVariableFrame>::const_reverse_iterator f = _local_frames.rbegin(); f != _local_frames.rend(); ++f) {
        const std::size_t first = f->first;
        for (std::size_t i = last; i > first; --i)
            locals = LispObjectAdder(LispAtom::New(*this, *_local_vars[i - 1].var)) + LispObjectAdder(locals);

        if (f->fenced)
            break;

        last = first;
    }
    aResult = LispSubList::New(LispObjectAdder(iList->Copy()) + LispObjectAdder(locals));
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

std::ostream& LispEnvironment::CurrentOutput()
{
    return *iCurrentOutput;
}

void LispEnvironment::SetCurrentOutput(std::ostream& aOutput)
{
    iCurrentOutput = &aOutput;
}



LispUserFunction* LispEnvironment::UserFunction(LispPtr& aArguments)
{
    auto i = iUserFunctions.find(aArguments->String());
    if (i != iUserFunctions.end()) {
        LispMultiUserFunction* multiUserFunc = &i->second;
        int arity = InternalListLength(aArguments)-1;
        return  multiUserFunc->UserFunc(arity);
    }
    return nullptr;
}


LispUserFunction* LispEnvironment::UserFunction(const LispString* aName, int aArity)
{
    auto i = iUserFunctions.find(aName);
    if (i != iUserFunctions.end())
        return i->second.UserFunc(aArity);

    return nullptr;
}



void LispEnvironment::UnFenceRule(const LispString* aOperator, int aArity)
{
    if (Protected(aOperator))
        throw LispErrProtectedSymbol(*aOperator);

    auto i = iUserFunctions.find(aOperator);

    if (i == iUserFunctions.end())
        throw LispErrInvalidArg();

    LispMultiUserFunction* multiUserFunc = &i->second;

    LispUserFunction* userFunc = multiUserFunc->UserFunc(aArity);

    if (!userFunc)
        throw LispErrInvalidArg();

    userFunc->UnFence();
}

void LispEnvironment::Retract(const LispString* aOperator, int aArity)
{
    if (Protected(aOperator))
        throw LispErrProtectedSymbol(*aOperator);

    auto i = iUserFunctions.find(aOperator);

    if (i != iUserFunctions.end())
        i->second.DeleteBase(aArity);
}

void LispEnvironment::DeclareRuleBase(const LispString* aOperator,
                                      LispPtr& aParameters,
                                      int aListed)
{
    if (Protected(aOperator))
        throw LispErrProtectedSymbol(*aOperator);

    LispMultiUserFunction* multiUserFunc = MultiUserFunction(aOperator);

    /*
     if (multiUserFunc->iFileToOpen)
    {
        LISPASSERT(multiUserFunc->iFileToOpen->iIsLoaded);
        }
        */

    // add an operator with this arity to the multiuserfunc.
    BranchingUserFunction* newFunc = 
            aListed
            ? new ListedBranchingUserFunction(aParameters) 
            : new BranchingUserFunction(aParameters);

    multiUserFunc->DefineRuleBase(newFunc);
}

void LispEnvironment::DeclareMacroRuleBase(const LispString* aOperator, LispPtr& aParameters, int aListed)
{
    if (Protected(aOperator))
        throw LispErrProtectedSymbol(*aOperator);

    LispMultiUserFunction* multiUserFunc = MultiUserFunction(aOperator);

    MacroUserFunction* newFunc = 
            aListed 
            ? new ListedMacroUserFunction(aParameters) 
            : new MacroUserFunction(aParameters);

    multiUserFunc->DefineRuleBase(newFunc);
}




LispMultiUserFunction* LispEnvironment::MultiUserFunction(const LispString* aOperator)
{
    auto i = iUserFunctions.find(aOperator);

    if (i != iUserFunctions.end())
        return &i->second;

    LispMultiUserFunction newMulti;
    return &iUserFunctions.insert(std::make_pair(aOperator, newMulti)).first->second;
    //SetAssociation(newMulti, aOperator);
}


void LispEnvironment::HoldArgument(const LispString*  aOperator, const LispString* aVariable)
{
    auto i = iUserFunctions.find(aOperator);

    if (i == iUserFunctions.end())
        throw LispErrInvalidArg();

    LispMultiUserFunction* multiUserFunc = &i->second;

    multiUserFunc->HoldArgument(aVariable);
}

void LispEnvironment::Protect(const LispString* symbol)
{
    protected_symbols.insert(symbol);
}

void LispEnvironment::UnProtect(const LispString* symbol)
{
    protected_symbols.erase(symbol);
}

bool LispEnvironment::Protected(const LispString* symbol) const
{
    return protected_symbols.find(symbol) != protected_symbols.end();
}

void LispEnvironment::DefineRule(const LispString* aOperator,int aArity,
                                 int aPrecedence, LispPtr& aPredicate,
                                 LispPtr& aBody)
{
    if (Protected(aOperator))
        throw LispErrProtectedSymbol(*aOperator);

    // Find existing multiuser func.
    auto i = iUserFunctions.find(aOperator);

    if (i == iUserFunctions.end())
        throw LispErrCreatingRule();

    LispMultiUserFunction* multiUserFunc = &i->second;

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

void LispEnvironment::DefineRulePattern(const LispString* aOperator,int aArity,
                                        int aPrecedence, LispPtr& aPredicate,
                                        LispPtr& aBody)
{
//    if (Protected(aOperator))
//        throw LispErrProtectedSymbol(*aOperator);

    // Find existing multiuser func.
    auto i = iUserFunctions.find(aOperator);

    if (i == iUserFunctions.end())
        throw LispErrCreatingRule();

    LispMultiUserFunction* multiUserFunc = &i->second;

    // Get the specific user function with the right arity
    LispUserFunction* userFunc = multiUserFunc->UserFunc(aArity);

    if (!userFunc)
        throw LispErrCreatingRule();

    // Declare a new evaluation rule
    userFunc->DeclarePattern(aPrecedence, aPredicate,aBody);
}

void LispEnvironment::SetCommand(YacasEvalCaller aEvaluatorFunc, const char* aString,int aNrArgs,int aFlags)
{
  const LispString* name = HashTable().LookUp(aString);
  YacasEvaluator eval(aEvaluatorFunc,aNrArgs,aFlags);
  auto i = iCoreCommands.find(name);
  if (i != iCoreCommands.end())
      i->second = eval;
  else
      iCoreCommands.insert(std::make_pair(name, eval));
}

void LispEnvironment::RemoveCoreCommand(char* aString)
{
  iCoreCommands.erase(HashTable().LookUp(aString));
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
  if (iUserFunc!=nullptr)
    iUserFunc->Trace();
}
LispLocalTrace::~LispLocalTrace()
{
  if (iUserFunc!=nullptr)
    iUserFunc->UnTrace();
}


