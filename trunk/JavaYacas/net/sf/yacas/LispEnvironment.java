package net.sf.yacas;


class LispEnvironment
{
  //TODO FIXME

  LispEnvironment(LispOutput aCurrentOutput/*TODO FIXME*/) throws Exception
  {
    iCurrentTokenizer = iDefaultTokenizer;
    iInitialOutput = aCurrentOutput;
    iCurrentOutput = aCurrentOutput;
    iCurrentPrinter = new InfixPrinter(iPrefixOperators, iInfixOperators, iPostfixOperators, iBodiedOperators);

    iTrue = LispAtom.New(this,"True");
    iFalse = LispAtom.New(this,"False");

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

  int iEvalDepth = 0;
  int iMaxEvalDepth = 10000;


  /** YacasArgStack implements a stack of pointers to objects that can be used to pass
  *  arguments to functions, and receive results back.
  */
  class YacasArgStack
  {
 
    //TODO appropriate constructor?
    public YacasArgStack(int aStackSize)
    {
      iStack = new LispPtrArray(aStackSize,null);
      iStackTop = 0;
//printf("STACKSIZE %d\n",aStackSize);
    }
    public int GetStackTop()  {return iStackTop;}
    public void RaiseStackOverflowError() throws Exception
    {
      LispError.RaiseError("Argument stack reached maximum. Please extend argument stack with --stack argument on the command line.");
    }
    public void PushArgOnStack(LispObject aObject) throws Exception
    {
      if (iStackTop >= iStack.Size())
      {
        RaiseStackOverflowError();
      }
      iStack.SetElement(iStackTop,aObject);
      iStackTop++;
    }

    public void PushNulls(int aNr)  throws Exception
    {
      if (iStackTop+aNr > iStack.Size())
      {
        RaiseStackOverflowError();
      }
      iStackTop+=aNr;
    }

    public LispPtr GetElement(int aPos) throws Exception
    {
      LispError.LISPASSERT(aPos>=0 && aPos < iStackTop);
      return iStack.GetElement(aPos);
    }
    public void PopTo(int aTop) throws Exception
    {
      LispError.LISPASSERT(aTop<=iStackTop);
      while (iStackTop>aTop)
      {
        iStackTop--;
        iStack.SetElement(iStackTop,null);
      }
    }
    LispPtrArray iStack;
    int iStackTop;
  };

  YacasArgStack iStack;


  public YacasCoreCommands CoreCommands()
  {
    return iCoreCommands;
  }
  YacasCoreCommands iCoreCommands = new YacasCoreCommands();

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
    LispGlobalVariable global = new LispGlobalVariable(aValue);
    iGlobals.SetAssociation(global, aVariable);
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
    LispGlobalVariable l = (LispGlobalVariable)iGlobals.LookUp(aVariable);
    if (l != null)
    {
      if (l.iEvalBeforeReturn)
      {
        iEvaluator.Eval(this, aResult, l.iValue);
        l.iValue.Set(aResult.Get());
        l.iEvalBeforeReturn = false;
        return;
      }
      else
      {
        aResult.Set(l.iValue.Get());
        return;
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
    iGlobals.Release(aString);
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

  LispGlobal iGlobals = new LispGlobal();

  boolean iSecure = false;

  int iLastUniqueId = 1;
  public int GetUniqueId()
  {
    return iLastUniqueId++;
  }

  LispOutput iCurrentOutput = null;
  LispOutput iInitialOutput = null;

  LispPrinter iCurrentPrinter = null;
  LispInput   iCurrentInput   = null;
  InputStatus iInputStatus    = new InputStatus();
  LispTokenizer iCurrentTokenizer;
  LispTokenizer iDefaultTokenizer = new LispTokenizer();
  LispTokenizer iXmlTokenizer = new XmlTokenizer();

  LispUserFunctions iUserFunctions = new LispUserFunctions();
 
  String iError = null;
 
  public void HoldArgument(String  aOperator, String aVariable) throws Exception
  {
    LispMultiUserFunction multiUserFunc = (LispMultiUserFunction)iUserFunctions.LookUp(aOperator);
    LispError.Check(multiUserFunc != null,LispError.KLispErrInvalidArg);
    multiUserFunc.HoldArgument(aVariable);
  }
  public void Retract(String aOperator,int aArity) throws Exception
  {
    LispMultiUserFunction multiUserFunc = (LispMultiUserFunction)iUserFunctions.LookUp(aOperator);
    if (multiUserFunc != null)
    {
      multiUserFunc.DeleteBase(aArity);
    }
  }

  public LispUserFunction UserFunction(LispPtr aArguments) throws Exception
  {
    LispMultiUserFunction multiUserFunc =
        (LispMultiUserFunction)iUserFunctions.LookUp(aArguments.Get().String());
    if (multiUserFunc != null)
    {
      int arity = LispStandard.InternalListLength(aArguments)-1;
      return  multiUserFunc.UserFunc(arity);
    }
    return null;
  }

  public LispUserFunction UserFunction(String aName,int aArity) throws Exception
  {
    LispMultiUserFunction multiUserFunc = (LispMultiUserFunction)iUserFunctions.LookUp(aName);
    if (multiUserFunc != null)
    {
        return  multiUserFunc.UserFunc(aArity);
    }
    return null;
  }

  public void UnFenceRule(String aOperator,int aArity) throws Exception
  {
    LispMultiUserFunction multiUserFunc = (LispMultiUserFunction)iUserFunctions.LookUp(aOperator);

    LispError.Check(multiUserFunc != null, LispError.KLispErrInvalidArg);
    LispUserFunction userFunc = multiUserFunc.UserFunc(aArity);
    LispError.Check(userFunc != null, LispError.KLispErrInvalidArg);
    userFunc.UnFence();
  }
 
  public LispMultiUserFunction MultiUserFunction(String aOperator) throws Exception
  {
    // Find existing multiuser func.
    LispMultiUserFunction multiUserFunc = (LispMultiUserFunction)iUserFunctions.LookUp(aOperator);

    // If none exists, add one to the user functions list
    if (multiUserFunc == null)
    {
        LispMultiUserFunction newMulti = new LispMultiUserFunction();
        iUserFunctions.SetAssociation(newMulti, aOperator);
        multiUserFunc = (LispMultiUserFunction)iUserFunctions.LookUp(aOperator);
        LispError.Check(multiUserFunc != null, LispError.KLispErrCreatingUserFunction);
    }
    return multiUserFunc;
  }

 
  public void DeclareRuleBase(String aOperator, LispPtr aParameters, boolean aListed) throws Exception
  {
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
    // Find existing multiuser func.
    LispMultiUserFunction multiUserFunc =
        (LispMultiUserFunction)iUserFunctions.LookUp(aOperator);
    LispError.Check(multiUserFunc != null, LispError.KLispErrCreatingRule);

    // Get the specific user function with the right arity
    LispUserFunction userFunc = (LispUserFunction)multiUserFunc.UserFunc(aArity);
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
    // Find existing multiuser func.
    LispMultiUserFunction multiUserFunc = (LispMultiUserFunction)iUserFunctions.LookUp(aOperator);
    LispError.Check(multiUserFunc != null, LispError.KLispErrCreatingRule);

    // Get the specific user function with the right arity
    LispUserFunction userFunc = multiUserFunc.UserFunc(aArity);
    LispError.Check(userFunc != null, LispError.KLispErrCreatingRule);
 
    // Declare a new evaluation rule
    userFunc.DeclarePattern(aPrecedence, aPredicate,aBody);
  }


  LispDefFiles iDefFiles = new LispDefFiles();
  InputDirectories iInputDirectories = new InputDirectories();

  String iPrettyReader = null;
  String iPrettyPrinter = null;

}

