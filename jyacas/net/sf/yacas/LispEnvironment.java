package net.sf.yacas;

import java.io.Writer;
import java.util.Set;
import java.util.HashSet;
import java.util.HashMap;

import static net.sf.yacas.LispError.Check;

final class LispEnvironment
{
  //TODO FIXME

  LispEnvironment(Writer aCurrentOutput) throws Exception
  {
    iCurrentTokenizer = iDefaultTokenizer;
    iInitialOutput = aCurrentOutput;
    iCurrentOutput = aCurrentOutput;
    iCurrentPrinter = new InfixPrinter(iPrefixOperators, iInfixOperators, iPostfixOperators, iBodiedOperators);

    protected_symbols = new HashSet<>();

    iTrue = LispAtom.New(this,"True");
    iFalse = LispAtom.New(this,"False");

    Protect(iTrue.String());
    Protect(iTrue.String());

    iEndOfFile    = LispAtom.New(this,"EndOfFile");
    iEndStatement = LispAtom.New(this,";");
    iProgOpen     = LispAtom.New(this,"[");
    iProgClose    = LispAtom.New(this,"]");
    iNth          = LispAtom.New(this,"Nth");
    iBracketOpen  = LispAtom.New(this,"(");
    iBracketClose = LispAtom.New(this,")");
    iListOpen     = LispAtom.New(this,"{");
    iListClose    = LispAtom.New(this,"}");
    iComma        = LispAtom.New(this,",");
    iList         = LispAtom.New(this,"List");
    iProg         = LispAtom.New(this,"Prog");

    Protect(iList.String());
    Protect(iProg.String());

    Protect(iHashTable.LookUp("Infinity"));
    Protect(iHashTable.LookUp("Undefined"));

    iStack = new YacasArgStack(50000 /*TODO FIXME*/);
    MathCommands mc = new MathCommands();
    mc.AddCommands(this);
    mc=null;
    PushLocalFrame(true);
  }

  LispHashTable HashTable()
  {
    return iHashTable;
  }
  int Precision()
  {
    return iPrecision;
  }
  void SetPrecision(int aPrecision) throws Exception
  {
    iPrecision = aPrecision;    // precision in decimal digits
  }
  private int iPrecision = 10;

  LispHashTable iHashTable = new LispHashTable();
  LispObject iTrue;
  LispObject iFalse;

  LispObject iEndOfFile;
  LispObject iEndStatement;
  LispObject iProgOpen;
  LispObject iProgClose;
  LispObject iNth;
  LispObject iBracketOpen;
  LispObject iBracketClose;
  LispObject iListOpen;
  LispObject iListClose;
  LispObject iComma;
  LispObject iList;
  LispObject iProg;

  LispOperators iPrefixOperators = new LispOperators();
  LispOperators iInfixOperators = new LispOperators();
  LispOperators iPostfixOperators = new LispOperators();
  LispOperators iBodiedOperators = new LispOperators();

  Set<String> protected_symbols;

  int iEvalDepth = 0;
  int iMaxEvalDepth = 1000;


    /**
     * YacasArgStack implements a stack of pointers to objects that can be used
     * to pass arguments to functions, and receive results back.
     */
    class YacasArgStack {

        public YacasArgStack(int aStackSize) {
            maxSize = aStackSize;
            iStack = new java.util.ArrayList<>();
        }

        public int GetStackTop() {
            return iStack.size();
        }

        public void RaiseStackOverflowError() throws Exception {
            LispError.RaiseError("Argument stack reached maximum. Please extend argument stack with --stack argument on the command line.");
        }

        public void PushArgOnStack(LispObject aObject) throws Exception {
            if (iStack.size() >= maxSize)
                RaiseStackOverflowError();

            iStack.add(new LispPtr(aObject));
        }

        public void PushNulls(int aNr) throws Exception {
            if (iStack.size() + aNr > maxSize)
                RaiseStackOverflowError();

            for (int i = 0; i < aNr; ++i)
                iStack.add(null);
        }

        public LispPtr GetElement(int aPos) throws Exception {
            LispError.LISPASSERT(aPos >= 0 && aPos < iStack.size());
            return iStack.get(aPos);
        }

        public void PopTo(int aTop) throws Exception {
            LispError.LISPASSERT(aTop <= iStack.size());
            while (iStack.size() > aTop)
                iStack.remove(iStack.size() - 1);
        }

        java.util.ArrayList<LispPtr> iStack;
        int maxSize;
    };

  YacasArgStack iStack;


  public HashMap<String, YacasEvaluator> CoreCommands()
  {
    return iCoreCommands;
  }

  HashMap<String, YacasEvaluator> iCoreCommands = new HashMap<>();

  LispEvaluatorBase iEvaluator = new BasicEvaluator();







  LispPtr FindLocal(String aVariable) throws Exception
  {
    LispError.Check(iLocalsList != null,LispError.KLispErrInvalidStack);
//    Check(iLocalsList.iFirst != null,KLispErrInvalidStack);
    LispLocalVariable t = iLocalsList.iFirst;

    while (t != null)
    {
        if (t.iVariable == aVariable)
        {
            return t.iValue;
        }
        t = t.iNext;
    }
    return null;
  }

  void SetVariable(String aVariable, LispPtr aValue, boolean aGlobalLazyVariable) throws Exception
  {
    LispPtr local = FindLocal(aVariable);
    if (local != null)
    {
      local.Set(aValue.Get());
      return;
    }
    // FIXME: or should local variables be protected as well?
    Check(!IsProtected(aVariable), LispError.KLispErrSymbolProtected);

    LispGlobalVariable global = new LispGlobalVariable(aValue);
    iGlobals.put(aVariable, global);
    if (aGlobalLazyVariable)
    {
      global.SetEvalBeforeReturn(true);
    }
  }

  void GetVariable(String aVariable,LispPtr aResult) throws Exception
  {
    aResult.Set(null);
    LispPtr local = FindLocal(aVariable);
    if (local != null)
    {
      aResult.Set(local.Get());
      return;
    }
    LispGlobalVariable l = iGlobals.get(aVariable);
    if (l != null)
    {
      if (l.iEvalBeforeReturn)
      {
        iEvaluator.Eval(this, aResult, l.iValue);
        l.iValue.Set(aResult.Get());
        l.iEvalBeforeReturn = false;
      }
      else
      {
        aResult.Set(l.iValue.Get());
      }
    }
  }

  void UnsetVariable(String aString) throws Exception
  {
    LispPtr local = FindLocal(aString);
    if (local != null)
    {
        local.Set(null);
        return;
    }
    // FIXME: or should local variables be protected as well?
    Check(!IsProtected(aString), LispError.KLispErrSymbolProtected);
    iGlobals.remove(aString);
  }

  void PushLocalFrame(boolean aFenced)
  {
    if (aFenced)
    {
        LocalVariableFrame newFrame =
            new LocalVariableFrame(iLocalsList, null);
        iLocalsList = newFrame;
    }
    else
    {
        LocalVariableFrame newFrame =
            new LocalVariableFrame(iLocalsList, iLocalsList.iFirst);
        iLocalsList = newFrame;
    }
  }

  void PopLocalFrame() throws Exception
  {
    LispError.LISPASSERT(iLocalsList != null);
    LocalVariableFrame nextFrame = iLocalsList.iNext;
    iLocalsList.Delete();
    iLocalsList = nextFrame;
  }

  void NewLocal(String aVariable,LispObject aValue) throws Exception
  {
    LispError.LISPASSERT(iLocalsList != null);
    iLocalsList.Add(new LispLocalVariable(aVariable, aValue));
  }


  class LispLocalVariable
  {
    public LispLocalVariable(String aVariable, LispObject aValue)
    {
      iNext = null;
      iVariable = aVariable;
      iValue.Set(aValue);

    }
    LispLocalVariable iNext;
    String iVariable;
    LispPtr iValue = new LispPtr();
  }
  class LocalVariableFrame
  {

      public LocalVariableFrame(LocalVariableFrame aNext, LispLocalVariable aFirst)
      {
        iNext = aNext;
        iFirst = aFirst;
        iLast = aFirst;
      }
      void Add(LispLocalVariable aNew)
      {
          aNew.iNext = iFirst;
          iFirst = aNew;
      }
      void Delete()
      {
        LispLocalVariable t = iFirst;
        LispLocalVariable next;
        while (t != iLast)
        {
          next = t.iNext;
          t = next;
        }
      }


      LocalVariableFrame iNext;
      LispLocalVariable iFirst;
      LispLocalVariable iLast;
  }
  LocalVariableFrame iLocalsList;

  HashMap<String, LispGlobalVariable> iGlobals = new HashMap<>();

  boolean iSecure = false;

  int iLastUniqueId = 1;
  public int GetUniqueId()
  {
    return iLastUniqueId++;
  }

  Writer iCurrentOutput = null;
  Writer iInitialOutput = null;

  LispPrinter iCurrentPrinter = null;
  LispInput   iCurrentInput   = null;
  InputStatus iInputStatus    = new InputStatus();
  LispTokenizer iCurrentTokenizer;
  LispTokenizer iDefaultTokenizer = new LispTokenizer();
  LispTokenizer iXmlTokenizer = new XmlTokenizer();

  HashMap<String, LispMultiUserFunction> iUserFunctions = new HashMap<>();

  String iError = null;

  public void HoldArgument(String  aOperator, String aVariable) throws Exception
  {
    LispMultiUserFunction multiUserFunc = iUserFunctions.get(aOperator);
    LispError.Check(multiUserFunc != null,LispError.KLispErrInvalidArg);
    multiUserFunc.HoldArgument(aVariable);
  }
  public void Retract(String aOperator,int aArity) throws Exception
  {
    Check(!IsProtected(aOperator), LispError.KLispErrSymbolProtected);
    LispMultiUserFunction multiUserFunc = iUserFunctions.get(aOperator);
    if (multiUserFunc != null)
    {
      multiUserFunc.DeleteBase(aArity);
    }
  }



  public LispUserFunction UserFunction(LispPtr aArguments) throws Exception
  {
    LispMultiUserFunction multiUserFunc =
        iUserFunctions.get(aArguments.Get().String());
    if (multiUserFunc != null)
    {
      int arity = LispStandard.InternalListLength(aArguments)-1;
      return  multiUserFunc.UserFunc(arity);
    }
    return null;
  }

  public LispUserFunction UserFunction(String aName,int aArity) throws Exception
  {
    LispMultiUserFunction multiUserFunc = iUserFunctions.get(aName);
    if (multiUserFunc != null)
    {
        return  multiUserFunc.UserFunc(aArity);
    }
    return null;
  }

  public void UnFenceRule(String aOperator,int aArity) throws Exception
  {
    Check(!IsProtected(aOperator), LispError.KLispErrSymbolProtected);
    LispMultiUserFunction multiUserFunc = iUserFunctions.get(aOperator);

    LispError.Check(multiUserFunc != null, LispError.KLispErrInvalidArg);
    LispUserFunction userFunc = multiUserFunc.UserFunc(aArity);
    LispError.Check(userFunc != null, LispError.KLispErrInvalidArg);
    userFunc.UnFence();
  }

  public LispMultiUserFunction MultiUserFunction(String aOperator) throws Exception
  {
    // Find existing multiuser func.
    LispMultiUserFunction multiUserFunc = iUserFunctions.get(aOperator);

    // If none exists, add one to the user functions list
    if (multiUserFunc == null)
    {
        multiUserFunc = new LispMultiUserFunction();
        iUserFunctions.put(aOperator, multiUserFunc);
    }
    return multiUserFunc;
  }


  public void DeclareRuleBase(String aOperator, LispPtr aParameters, boolean aListed) throws Exception
  {
    Check(!IsProtected(aOperator), LispError.KLispErrSymbolProtected);

    LispMultiUserFunction multiUserFunc = MultiUserFunction(aOperator);

    // add an operator with this arity to the multiuserfunc.
    BranchingUserFunction newFunc;
    if (aListed)
    {
        newFunc = new ListedBranchingUserFunction(aParameters);
    }
    else
    {
        newFunc = new BranchingUserFunction(aParameters);
    }
    multiUserFunc.DefineRuleBase(newFunc);
  }

  public void DefineRule(String aOperator,int aArity,
                                  int aPrecedence, LispPtr aPredicate,
                                  LispPtr aBody) throws Exception
  {
    Check(!IsProtected(aOperator), LispError.KLispErrSymbolProtected);

    // Find existing multiuser func.
    LispMultiUserFunction multiUserFunc = iUserFunctions.get(aOperator);
    LispError.Check(multiUserFunc != null, LispError.KLispErrCreatingRule);

    // Get the specific user function with the right arity
    LispUserFunction userFunc = multiUserFunc.UserFunc(aArity);
    LispError.Check(userFunc != null, LispError.KLispErrCreatingRule);

    // Declare a new evaluation rule


    if (LispStandard.IsTrue(this, aPredicate))
    {
//        printf("FastPredicate on %s\n",aOperator->String());
        userFunc.DeclareRule(aPrecedence, aBody);
    }
    else
        userFunc.DeclareRule(aPrecedence, aPredicate,aBody);
  }

  void DeclareMacroRuleBase(String aOperator, LispPtr aParameters, boolean aListed) throws Exception
  {
    Check(!IsProtected(aOperator), LispError.KLispErrSymbolProtected);

    LispMultiUserFunction multiUserFunc = MultiUserFunction(aOperator);
    MacroUserFunction newFunc;
    if (aListed)
    {
      newFunc = new ListedMacroUserFunction(aParameters);
    }
    else
    {
      newFunc = new MacroUserFunction(aParameters);
    }
    multiUserFunc.DefineRuleBase(newFunc);
  }

  void DefineRulePattern(String aOperator,int aArity, int aPrecedence, LispPtr aPredicate, LispPtr aBody) throws Exception
  {
    // Check(!IsProtected(aOperator), LispError.KLispErrSymbolProtected);

    // Find existing multiuser func.
    LispMultiUserFunction multiUserFunc = iUserFunctions.get(aOperator);
    LispError.Check(multiUserFunc != null, LispError.KLispErrCreatingRule);

    // Get the specific user function with the right arity
    LispUserFunction userFunc = multiUserFunc.UserFunc(aArity);
    LispError.Check(userFunc != null, LispError.KLispErrCreatingRule);

    // Declare a new evaluation rule
    userFunc.DeclarePattern(aPrecedence, aPredicate,aBody);
  }


  void Protect(String symbol)
  {
      protected_symbols.add(symbol);
  }

  void UnProtect(String symbol)
  {
      protected_symbols.remove(symbol);
  }

  boolean IsProtected(String symbol)
  {
      return protected_symbols.contains(symbol);
  }

  LispDefFiles iDefFiles = new LispDefFiles();
  InputDirectories iInputDirectories = new InputDirectories();

  String iPrettyReader = null;
  String iPrettyPrinter = null;

}
