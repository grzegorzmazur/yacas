


void LispLexCompare2(LispEnvironment& aEnvironment, int aStackTop,
                     boolean (*lexfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,int aPrecision),
#ifndef NO_USE_BIGFLOAT
                     boolean (*numfunc)(BigNumber& n1, BigNumber& n2)
#else
                     boolean (*numfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,int aPrecision)
#endif
                    );





/* StrCompare returns f1-f2: if f1 < f2 it returns -1, if f1=f2 it
 returns 0, and it returns 1 if f1>f2
 */
// the aPrecision argument is ignored here
static boolean LexLessThan(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,int aPrecision)
{
    return (StrCompare(f1, f2)<0);
}

// the aPrecision argument is ignored here
static boolean LexGreaterThan(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,int aPrecision)
{
    return (StrCompare(f1, f2)>0);
}

#ifndef NO_USE_BIGFLOAT
static boolean BigLessThan(BigNumber& n1, BigNumber& n2)
{
  return n1.LessThan(n2) && !n1.Equals(n2);
}
static boolean BigGreaterThan(BigNumber& n1, BigNumber& n2)
{
  return !(n1.LessThan(n2) || n1.Equals(n2));
}
#endif

void LispLessThan(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
    LispLexCompare2(aEnvironment, aStackTop, LexLessThan,BigLessThan);
#else
    LispLexCompare2(aEnvironment, aStackTop, LexLessThan,LessThan);
#endif
}

void LispGreaterThan(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
    LispLexCompare2(aEnvironment, aStackTop, LexGreaterThan, BigGreaterThan);
#else
    LispLexCompare2(aEnvironment, aStackTop, LexGreaterThan, GreaterThan);
#endif
}


void LispLexCompare2(LispEnvironment& aEnvironment, int aStackTop,
                     boolean (*lexfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,int aPrecision),
#ifndef NO_USE_BIGFLOAT
                     boolean (*numfunc)(BigNumber& n1, BigNumber& n2)
#else
                     boolean (*numfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,int aPrecision)
#endif
                    )
{
    //TESTARGS(3);
    LispPtr result1 = new LispPtr();
    LispPtr result2 = new LispPtr();
    result1.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
    result2.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
    boolean cmp;
#ifndef NO_USE_BIGFLOAT
    RefPtr<BigNumber> n1; n1 = result1.Get().Number(aEnvironment.Precision());
    RefPtr<BigNumber> n2; n2 = result2.Get().Number(aEnvironment.Precision());
    if (n1.Ptr() != null && n2.Ptr() != null)
    {
      cmp =numfunc(*n1.Ptr(),*n2.Ptr());
    }
#else
    String str1;
    String str2;
    str1 = result1.Get().String();
    str2 = result2.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str1 != null ,1);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str2 != null, 2);
    if (IsNumber(str1.String(),true) &&
        IsNumber(str2.String(),true))
    {
      cmp =numfunc(str1.String(),str2.String(),
                            aEnvironment.HashTable(),
                            aEnvironment.Precision());
    }
#endif
    else
    {
#ifndef NO_USE_BIGFLOAT
      String str1;
      String str2;
      str1 = result1.Get().String();
      str2 = result2.Get().String();
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str1 != null ,1);
      LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str2 != null, 2);
#endif
	  // the precision argument is ignored in "lex" functions
      cmp =lexfunc(str1.String(),str2.String(),
                            aEnvironment.HashTable(),
                            aEnvironment.Precision());
    }
    
    LispStandard.InternalBoolean(aEnvironment,RESULT(aEnvironment, aStackTop), cmp);
}

// this function will eventually be moved to scripts
void LispPi(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(1);
    RESULT(aEnvironment, aStackTop).Set(PiFloat(aEnvironment, aEnvironment.Precision()));
}





/// Implements the Yacas functions \c RuleBase and \c MacroRuleBase .
/// The real work is done by LispEnvironment::DeclareRuleBase().
static void InternalRuleBase(LispEnvironment& aEnvironment, int aStackTop, 
                             int aListed)
{
    //TESTARGS(3);
    
    // Get operator
    LispPtr args = new LispPtr();
    String orig=null;
    
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
    orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    args.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
    
    // The arguments
    CHK_ISLIST_CORE(args,2);

    // Finally define the rule base
    aEnvironment.DeclareRuleBase(LispStandard.SymbolName(aEnvironment,orig.String()),
                                 args.Get().SubList().Get().Next(),aListed);
    
    // Return true
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}

/// Corresponds to the Yacas function \c RuleBase .
/// This function simply calls InternalRuleBase().
void LispRuleBase(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalRuleBase(aEnvironment, aStackTop, false);
}
void LispMacroRuleBase(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalRuleBase(aEnvironment, aStackTop, false);
}

void InternalDefMacroRuleBase(LispEnvironment& aEnvironment, int aStackTop, int aListed)

{
    //TESTARGS(3);
    
    // Get operator
    LispPtr args = new LispPtr();
    LispPtr body = new LispPtr();
    String orig=null;
    
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
    orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

    // The arguments
    args.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
    CHK_ISLIST_CORE(args,2);

    // Finally define the rule base
    aEnvironment.DeclareMacroRuleBase(LispStandard.SymbolName(aEnvironment,orig.String()),
                                 args.Get().SubList().Get().Next(),aListed);
    
    // Return true
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));

}

void LispDefMacroRuleBaseListed(LispEnvironment& aEnvironment, int aStackTop)
{
  InternalDefMacroRuleBase(aEnvironment, aStackTop, true);
}
void LispDefMacroRuleBase(LispEnvironment& aEnvironment, int aStackTop)
{
  InternalDefMacroRuleBase(aEnvironment, aStackTop, false);
}




void LispDllDirectory(LispEnvironment& aEnvironment, int aStackTop)
{
    // Get file name
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
    String orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    LispString oper;
    InternalUnstringify(oper, orig);
    aEnvironment.iDllDirectories.Append(new LispString(oper.String()));
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}


void LispDefLoad(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(2);
    LispError.CHK_CORE(aEnvironment, aStackTop,aEnvironment.iSecure == 0, LispError.KLispErrSecurityBreach);

    LispPtr evaluated = new LispPtr();
    evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

    // Get file name
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
    String orig = evaluated.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);

    LoadDefFile(aEnvironment, orig);
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}


void LispFindFile(LispEnvironment& aEnvironment,int aStackTop)
{

    //TESTARGS(2);

    LispError.CHK_CORE(aEnvironment, aStackTop,aEnvironment.iSecure == 0, LispError.KLispErrSecurityBreach);
    
    LispPtr evaluated = new LispPtr();
    evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

    // Get file name
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
    String orig = evaluated.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispChar filename[1024];//TODO FIXME
    InternalFindFile(oper.String(), aEnvironment.iInputDirectories,
                     filename);
    LispString res(filename,1);
    RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpStringify(res.String()).String()));
}



void LispCustomEval(LispEnvironment& aEnvironment,int aStackTop)
{
  //TESTARGS(5);
  if (aEnvironment.iDebugger) delete aEnvironment.iDebugger;
  aEnvironment.iDebugger = new DefaultDebugger(ARGUMENT(aEnvironment, aStackTop, 1), ARGUMENT(aEnvironment, aStackTop, 2),ARGUMENT(aEnvironment, aStackTop, 3));
  LispLocalEvaluator local(aEnvironment,new TracedEvaluator);
  aEnvironment.iDebugger.Start();
  aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 4));
  aEnvironment.iDebugger.Finish();
  delete aEnvironment.iDebugger;
  aEnvironment.iDebugger = null;
}

void LispCustomEvalExpression(LispEnvironment& aEnvironment,int aStackTop)
{
  //TESTARGS(1);
  if (aEnvironment.iDebugger == null)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  RESULT(aEnvironment, aStackTop).Set(aEnvironment.iDebugger.iTopExpr.Get()); 
}
void LispCustomEvalResult(LispEnvironment& aEnvironment,int aStackTop)
{
  //TESTARGS(1);
  if (aEnvironment.iDebugger == null)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  RESULT(aEnvironment, aStackTop).Set(aEnvironment.iDebugger.iTopResult.Get()); 
}



void LispCustomEvalLocals(LispEnvironment& aEnvironment,int aStackTop)
{
  //TESTARGS(1);
  aEnvironment.CurrentLocals(RESULT(aEnvironment, aStackTop));
}

void LispCustomEvalStop(LispEnvironment& aEnvironment,int aStackTop)
{
  //TESTARGS(1);
  if (aEnvironment.iDebugger == null)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  aEnvironment.iDebugger.iStopped = true;

  LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}

void LispTraceStack(LispEnvironment& aEnvironment,int aStackTop)
{
    //TESTARGS(2);
    LispLocalEvaluator local(aEnvironment,new TracedStackEvaluator);
    aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 1));
}


void LispTraceRule(LispEnvironment& aEnvironment,int aStackTop)
{
    //TESTARGS(3);
    LispPtr *ptr = ARGUMENT(aEnvironment, aStackTop, 0).Get().Next().Get().SubList();
    LispUserFunction* userfunc=null;
    if (ptr != null)
        userfunc = GetUserFunction(aEnvironment,ptr);
    LispLocalTrace trace(userfunc);
    aEnvironment.iEvaluator.Eval(aEnvironment, RESULT(aEnvironment, aStackTop), ARGUMENT(aEnvironment, aStackTop, 2));
}



void LispFindFunction(LispEnvironment& aEnvironment,int aStackTop)
{
    //TESTARGS(2);
    LispError.CHK_CORE(aEnvironment, aStackTop,aEnvironment.iSecure == 0, LispError.KLispErrSecurityBreach);
    
    LispPtr evaluated = new LispPtr();
    evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

    // Get file name
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,evaluated.Get() != null, 1);
    String orig = evaluated.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper.String()));
    if (multiUserFunc != null)
    {
        LispDefFile* def = multiUserFunc.iFileToOpen;
        if (def != null)
        {
            RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,def.iFileName().String()));
            return;
        }
    }
    RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,"\"\""));
}

void LispDefLoadFunction(LispEnvironment& aEnvironment,int aStackTop)
{
    //TESTARGS(2);
    LispPtr name = new LispPtr();
    name.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
    String orig = name.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper.String()));
    if (multiUserFunc != null)
    {
        if (multiUserFunc.iFileToOpen!=null)
        {
            LispDefFile* def = multiUserFunc.iFileToOpen;
            if (!def.iIsLoaded)
            {
#ifdef YACAS_DEBUG
                /*Show loading... */
              {
                extern int verbose_debug;
                if (verbose_debug)
                  printf("Debug> Loading file %s for function %s\n",def.iFileName().String(),oper.String());
              }
#endif
                multiUserFunc.iFileToOpen=null;
                InternalUse(aEnvironment,def.iFileName());
            }
        }
    }
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}


void LispRuleBaseArgList(LispEnvironment& aEnvironment,int aStackTop)
{
    //TESTARGS(3);
    LispPtr name = new LispPtr();
    name.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
    String orig = name.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispPtr sizearg = new LispPtr();
    sizearg.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get() != null, 2);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,sizearg.Get().String() != null, 2);

    int arity = Integer.parseInt(sizearg.Get().String(),10);

    LispUserFunction* userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper.String()),arity);
    LispError.CHK_CORE(aEnvironment, aStackTop,userFunc != null, LispError.KLispErrInvalidArg);

    LispPtr& list = userFunc.ArgList();
    LispPtr head = new LispPtr();
    head.Set(aEnvironment.iList.Copy(false));
    head.Get().Next().Set(list.Get());
    RESULT(aEnvironment, aStackTop).Set(LispSubList.New(head.Get()));
}


static void InternalNewRulePattern(LispEnvironment& aEnvironment, int aStackTop, boolean aMacroMode)
{
    //TESTARGS(6);

    int arity;
    int precedence;

    LispPtr ar = new LispPtr();
    LispPtr pr = new LispPtr();
    LispPtr predicate = new LispPtr();
    LispPtr body = new LispPtr();
    String orig=null;
    
    // Get operator
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get() != null, 1);
    orig = ARGUMENT(aEnvironment, aStackTop, 1).Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,orig != null, 1);
    ar.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
    pr.Set(ARGUMENT(aEnvironment, aStackTop, 3).Get());
    predicate.Set(ARGUMENT(aEnvironment, aStackTop, 4).Get());
    body.Set(ARGUMENT(aEnvironment, aStackTop, 5).Get());
    
    // The arity
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ar.Get() != null, 2);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ar.Get().String() != null, 2);
    arity = Integer.parseInt(ar.Get().String(),10);

    // The precedence
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,pr.Get() != null, 3);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,pr.Get().String() != null, 3);
    precedence = Integer.parseInt(pr.Get().String(),10);
    
    // Finally define the rule base
    aEnvironment.DefineRulePattern(LispStandard.SymbolName(aEnvironment,orig.String()),
                            arity,
                            precedence,
                            predicate,
                            body );

    // Return true
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}

void LispNewRulePattern(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalNewRulePattern(aEnvironment, aStackTop, false);
}

void LispMacroNewRulePattern(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalNewRulePattern(aEnvironment, aStackTop, true);
}








#ifndef NO_USE_BIGFLOAT

/// Construct a BigNumber from one of the arguments.
/// \param x (on output) the constructed bignumber
/// \param aEnvironment the current environment
/// \param aStackTop the index of the top of the stack
/// \param aArgNr the index of the argument to be converted
void GetNumber(RefPtr<BigNumber>& x, LispEnvironment& aEnvironment, int aStackTop, int aArgNr)
{
    /*??? */
    x = ARGUMENT(aEnvironment, aStackTop, aArgNr).Get().Number(aEnvironment.Precision());
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,x.Ptr() != null,aArgNr);
/*TODO remove old?
    RefPtr<BigNumber> num; 
    num = ARGUMENT(aEnvironment, aStackTop, aArgNr).Get().Number(aEnvironment.Precision());
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,num.Ptr() != null,aArgNr);
    x = num;
*/
}
#endif // USE_BIGFLOAT

//FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment, int aStackTop,
                     LispObject* (*func)(LispObject* f1, LispObject* f2,LispEnvironment& aEnvironment,int aPrecision),
                    boolean arbbase=false);

void LispArithmetic1(LispEnvironment& aEnvironment, int aStackTop,
                     LispObject* (*func)(LispObject* f1, LispEnvironment& aEnvironment,int aPrecision));






//FIXME remove these
void LispArithmetic1(LispEnvironment& aEnvironment, int aStackTop,
                     LispObject* (*func)(LispObject* f1, LispEnvironment& aEnvironment,int aPrecision))
{
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get().Number(0),1);
    RESULT(aEnvironment, aStackTop).Set(func(ARGUMENT(aEnvironment, aStackTop, 1).Get(), aEnvironment, aEnvironment.Precision())); 
}


//FIXME remove these
void LispArithmetic2(LispEnvironment& aEnvironment, int aStackTop,
                     LispObject* (*func)(LispObject* f1, LispObject* f2,LispEnvironment& aEnvironment,int aPrecision),
                    boolean arbbase)
{
    if (!arbbase)
    {
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get().Number(0) ,1);
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 2).Get().Number(0) ,2);
    }
    RESULT(aEnvironment, aStackTop).Set(func(ARGUMENT(aEnvironment, aStackTop, 1).Get(),ARGUMENT(aEnvironment, aStackTop, 2).Get(),
                                   aEnvironment,
                                   aEnvironment.Precision())); 
}


void LispDumpBigNumberDebugInfo(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
  RefPtr<BigNumber> x;
  GetNumber(x,aEnvironment, aStackTop, 1);
  x.DumpDebugInfo();
#endif
  LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}


//TODO we need to have Gcd in BigNumber!
void LispGcd(LispEnvironment& aEnvironment, int aStackTop)
{
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get().Number(0) ,1);
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,ARGUMENT(aEnvironment, aStackTop, 1).Get().Number(0) ,2);
    RESULT(aEnvironment, aStackTop).Set(GcdInteger(ARGUMENT(aEnvironment, aStackTop, 1).Get(),ARGUMENT(aEnvironment, aStackTop, 2).Get(),aEnvironment));
}


void LispArcCos(LispEnvironment& aEnvironment, int aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, ArcCosFloat);
}

void LispArcTan(LispEnvironment& aEnvironment, int aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, ArcTanFloat);
}

void LispSqrt(LispEnvironment& aEnvironment, int aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, SqrtFloat);
}


#ifndef NO_USE_BIGFLOAT
#define UNARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, int aStackTop) \
{ \
      RefPtr<BigNumber> x; \
      GetNumber(x,aEnvironment, aStackTop, 1); \
      BigNumber *z = new BigNumber( /*???*/ *x.Ptr() /*TODO remove? aEnvironment.BinaryPrecision()*/); \
      z.BigNumName(/*???*/ *z /*TODO remove? *x.Ptr() */); \
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z)); \
}
#define BINARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, int aStackTop) \
{ \
      RefPtr<BigNumber> x; \
      RefPtr<BigNumber> y; \
      GetNumber(x,aEnvironment, aStackTop, 1); \
      GetNumber(y,aEnvironment, aStackTop, 2); \
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision()); \
      z.BigNumName(*x.Ptr(), *y.Ptr()); \
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z)); \
}
#else
#define UNARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, int aStackTop) \
{ \
    LispArithmetic1(aEnvironment, aStackTop, OldName); \
}
#define BINARYFUNCTION(LispName, BigNumName, OldName) \
void LispName(LispEnvironment& aEnvironment, int aStackTop) \
{ \
    LispArithmetic2(aEnvironment, aStackTop, OldName); \
}
#endif

UNARYFUNCTION(LispFloor, Floor, FloorFloat)
UNARYFUNCTION(LispMathNegate, Negate, NegateFloat)

/** the macro
	UNARYFUNCTION(LispFloor, Floor, FloorFloat)
is used to help interface Yacas with BigNumber. Suppose we need to access a unary function named 'Floor' in BigNumber and 'LispFloor' here, with 'FloorFloat' the backup function for no BigNumber support.
The macro produces the following equivalent code for the unary function:
void LispFloor(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision());
      z.Floor(*x.Ptr());
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
#else
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}
*/
/* FIXME Eventually the BigNumber support will be stable and we can remove old code and simplify these macros */

/// obtain internal precision data on a number object.
void LispGetExactBits(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision());
      z.SetTo(
	    (x.Ptr().IsInt())
		? x.Ptr().BitCount()	// for integers, return the bit count
	  	: x.Ptr().GetPrecision() 	// for floats, return the precision
      );
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
#else	// this is not defined without BigNumber, so return something
    RaiseError("Function MathGetExactBits is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}
/// set internal precision data on a number object.
void LispSetExactBits(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision());
      z.SetTo(*x.Ptr());
	  // do nothing for integers
	  if (!(z.IsInt()))
	    z.Precision((long)(y.Double()));	// segfaults unless y is defined?
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
#else	// this is not defined without BigNumber, so return something
    RaiseError("Function MathSetExactBits is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}


/// obtain the bit count of a number object.
void LispBitCount(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision());
      z.SetTo(x.Ptr().BitCount());
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
#else	// this is not defined without BigNumber, so return something
    RaiseError("Function MathBitCount is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}


/// check whether a number object fits into a platform type.
void LispMathIsSmall(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      if(x.Ptr().IsSmall())
	LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
      else
	LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));      
#else	// this is not defined without BigNumber, so return False
    LispStandard.InternalFalse(aEnvironment,RESULT(aEnvironment, aStackTop));
#endif
}


void LispCeil(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
/*
      int prec = aEnvironment.Precision();
      if (x.iNumber.TensExp > x.iNumber.iPrecision)
      {
        aEnvironment.SetPrecision(x.iNumber.iTensExp);
        GetNumber(x,aEnvironment, aStackTop, 1);
      }
*/
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision());
      z.Negate(*x.Ptr());
      z.Floor(*z);	// danger: possible exception raised in Floor() leads to a memory leak because z is not destroyed
      z.Negate(*z);
//      aEnvironment.SetPrecision(prec);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
#else
   LispArithmetic1(aEnvironment, aStackTop, CeilFloat);
#endif
}

//BINARYFUNCTION(LispMod, Mod, ModFloat)
/* this will be gone */
void LispMod(LispEnvironment& aEnvironment, int aStackTop)
{//FIXME
    LispArithmetic2(aEnvironment, aStackTop, ModFloat);
}
/* up to here */


void LispPower(LispEnvironment& aEnvironment, int aStackTop)
{//FIXME move to scripts
    LispArithmetic2(aEnvironment, aStackTop, PowerFloat);
}



void LispFac(LispEnvironment& aEnvironment, int aStackTop)
{//FIXME move to scripts
    LispArithmetic1(aEnvironment, aStackTop, LispFactorial);
}


// platform functions, taking/returning a platform int/float

void LispFastIsPrime(LispEnvironment& aEnvironment, int aStackTop)
{//FIXME
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      long result = primes_table_check((unsigned long)(x.Double()));
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision());
      z.SetTo(result);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
#else
    LispArithmetic1(aEnvironment, aStackTop, PlatIsPrime);
#endif
}

// define a macro to replace all platform math functions
#ifndef NO_USE_BIGFLOAT

  #ifndef HAVE_MATH_H
  // this warning is here just to be sure what we are compiling
    #warning do not have math.h
    #define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, int aStackTop) \
{ \
      LispBackupName(aEnvironment, aStackTop); \
}
    #define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, int aStackTop) \
{ \
      LispBackupName(aEnvironment, aStackTop); \
}
  #else	// HAVE_MATH_H
    #define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, int aStackTop) \
{ \
      RefPtr<BigNumber> x; \
      GetNumber(x,aEnvironment, aStackTop, 1); \
      double result = PlatformName(x.Double()); \
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision()); \
      z.SetTo(result); \
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z)); \
}
    #define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, int aStackTop) \
{ \
      RefPtr<BigNumber> x, y; \
      GetNumber(x,aEnvironment, aStackTop, 1); \
      GetNumber(y,aEnvironment, aStackTop, 2); \
      double result = PlatformName(x.Double(), y.Double()); \
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision()); \
      z.SetTo(result); \
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z)); \
}
  #endif
#else	// NO_USE_BIGFLOAT
  // this warning is here just to be sure what we are compiling
  #warning not using BigNumber:: functions
  #define PLATFORM_UNARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, int aStackTop) \
{ \
    LispArithmetic1(aEnvironment, aStackTop, OldName);
}
  #define PLATFORM_BINARY(LispName, PlatformName, LispBackupName, OldName) \
void LispName(LispEnvironment& aEnvironment, int aStackTop) \
{ \
    LispArithmetic2(aEnvironment, aStackTop, OldName);
}
#endif

// now we can define all such functions, e.g.:
//	PLATFORM_UNARY(LispFastSin, sin, LispSin, PlatSin)
// this will generate the following equivalent code:
/*

void LispFastSin(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
#ifdef HAVE_MATH_H
      RefPtr<BigNumber> x;
      GetNumber(x,aEnvironment, aStackTop, 1);
      double result = sin(x.Double());
      BigNumber *z = new BigNumber(aEnvironment.BinaryPrecision());
      z.SetTo(result);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
#else
      LispSin(aEnvironment, aStackTop);
#endif
      return;
#endif

    LispArithmetic1(aEnvironment, aStackTop, PlatSin);
}

*/

// some or all of these functions should be moved to scripts
	PLATFORM_BINARY(LispFastMod, fmod, LispMod, PlatMod)

/*
// BitNot not yet in yacasapi etc.
//BINARYFUNCTION(LispBitNot, BitNot, BitNot)
*/
/* this will be gone */
void LispShiftLeft(LispEnvironment& aEnvironment, int aStackTop)
{//FIXME
    LispArithmetic2(aEnvironment, aStackTop, ShiftLeft);
}
void LispShiftRight(LispEnvironment& aEnvironment, int aStackTop)
{//FIXME
    LispArithmetic2(aEnvironment, aStackTop, ShiftRight);
}


/* up to here */


void LispFromBase(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(3);

    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper = new LispPtr();
    oper.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num; num = oper.Get().Number(aEnvironment.BinaryPrecision());
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,num.Ptr() != null,1);
	// check that the base is an integer between 2 and 32
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,num.IsInt()
        && num.Double() >= BASE2 && num.Double() <= log2_table_range(), 1);

    // Get a short platform integer from the first argument
    int base = (int)(num.Double());

    // Get the number to convert
    LispPtr fromNum = new LispPtr();
    fromNum.Set(ARGUMENT(aEnvironment, aStackTop, 2).Get());
    String str2;
    str2 = fromNum.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str2 != null,2);

    // Added, unquote a string
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,InternalIsString(str2),2);
    str2 = aEnvironment.HashTable().LookUpUnStringify(str2.String());

    // convert using correct base
	// FIXME: API breach, must pass precision in base digits and not in bits!
	// if converting an integer, the precision argument is ignored,
	// but if converting a float, need to use bits_to_digits(BinaryPrecision, base)
    BigNumber *z = new BigNumber(str2.String(),aEnvironment.BinaryPrecision(),base);
    RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));
}
void LispToBase(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(3);

    // Get the base to convert to:
    // Evaluate first argument, and store result in oper
    LispPtr oper = new LispPtr();
    oper.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
    // Check that result is a number, and that it is in fact an integer
    RefPtr<BigNumber> num; num = oper.Get().Number(aEnvironment.BinaryPrecision());
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,num.Ptr() != null,1);
	// check that the base is an integer between 2 and 32
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,num.IsInt()
        && num.Double() >= BASE2 && num.Double() <= log2_table_range(), 1);

    // Get a short platform integer from the first argument
    int base = (int)(num.Double());

    // Get the number to convert
    RefPtr<BigNumber> x;
    GetNumber(x,aEnvironment, aStackTop, 2);

    // convert using correct base
    LispString str;
	// FIXME: API breach, must pass precision in base digits and not in bits!
	// if converting an integer, the precision argument is ignored,
	// but if converting a float, need to use bits_to_digits(BinaryPrecision, base)
    x.ToString(str,aEnvironment.BinaryPrecision(),base);
    // Get unique string from hash table, and create an atom from it.

    RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpStringify(str.String()).String()));
}




void LispPrettyPrinter(LispEnvironment& aEnvironment, int aStackTop)
{
    int nrArguments = InternalListLength(ARGUMENT(aEnvironment, aStackTop, 0));

    if (nrArguments == 1)
    {
        aEnvironment.SetPrettyPrinter(null);
    }
    else
    {
        LispError.CHK_CORE(aEnvironment, aStackTop,nrArguments == 2,LispError.KLispErrWrongNumberOfArgs);
        LispPtr oper = new LispPtr();
        oper.Set(ARGUMENT(aEnvironment, aStackTop, 0).Get());
        oper.GoNext();
/*TODO remove
        oper.Set(Argument(ARGUMENT(aEnvironment, aStackTop, 0),1).Get());
*/
        CHK_ISSTRING_CORE(oper,1);
        aEnvironment.SetPrettyPrinter(oper.Get().String());
    }
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}

void LispGetPrettyPrinter(LispEnvironment& aEnvironment, int aStackTop)
{
  if (aEnvironment.PrettyPrinter() == null)
    RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,"\"\""));
  else
    RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.PrettyPrinter().String()));
}

void LispPatchLoad(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(2);

    LispPtr evaluated = new LispPtr();
    evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

    String string = evaluated.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,string != null, 1);

    LispString oper;
    InternalUnstringify(oper, string);
    String hashedname = aEnvironment.HashTable().LookUp(oper.String());

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname.String());

    
    LispLocalFile localFP(aEnvironment, oper.String(),true,
                          aEnvironment.iInputDirectories);
    LispError.Check(localFP.iOpened != 0, LispError.KLispErrFileNotFound);
    FILEINPUT newInput(localFP,aEnvironment.iInputStatus);

    PatchLoad(newInput.StartPtr(),
              *aEnvironment.CurrentOutput(),
              aEnvironment);
    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}

void LispPatchString(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(2);

    LispPtr evaluated = new LispPtr();
    evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
    
    String string = evaluated.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,string != null, 1);
    LispString oper;
    InternalUnstringify(oper, string);

    LispString str;
    StringOutput newOutput(str);

    LispLocalOutput localOutput(aEnvironment, &newOutput);

    PatchLoad(&oper[0], newOutput, aEnvironment);

    RESULT(aEnvironment, aStackTop).Set(LispAtom.New(aEnvironment,aEnvironment.HashTable().LookUpStringify(str.String()).String()));
}

void LispDllLoad(LispEnvironment& aEnvironment, int aStackTop)
{
  //TESTARGS(2);

  LispPtr evaluated = new LispPtr();
  evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

  String string = evaluated.Get().String();
  LispError.CHK_ARG_CORE(aEnvironment,aStackTop,string != null, 1);

  LispString oper;
  InternalUnstringify(oper, string);
  LispDllBase *dll=null;
  int opened = false;

  ExePluginMaker maker = FindExePlugin(&oper[0]);
  if (maker != null)
  {
    dll  = new ExeDll(maker);
  }
#ifndef DISABLE_DYNAMIC
  else
  {
    dll  = new DLLCLASS;
  }
#endif // DISABLE_DYNAMIC
  LispError.Check(dll != null,LispError.KLispErrNotEnoughMemory);


#ifdef YACAS_DEBUG
  printf("DLL allocated\n");
#endif// YACAS_DEBUG
  opened = dll.Open(&oper[0],aEnvironment);
  if (!opened) delete dll;
#ifdef YACAS_DEBUG
  printf("DLL opened, opened=%d\n",opened);
#endif// YACAS_DEBUG
  LispError.Check(opened,LispError.KLispErrLibraryNotFound);
  aEnvironment.iDlls.Append(dll);

#ifdef YACAS_DEBUG
      printf("DLL added, %d DLLs loaded\n",aEnvironment.iDlls.Size());
#endif// YACAS_DEBUG

  LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}

void LispDllUnload(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(2);
    LispPtr evaluated = new LispPtr();
    evaluated.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());

    String string = evaluated.Get().String();
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,string != null, 1);

    LispString oper;
    InternalUnstringify(oper, string);
    aEnvironment.iDlls.DeleteNamed(&oper[0],aEnvironment);

    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}

void LispDllEnumerate(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(1);
    int i;
    LispObject *res = null;
    for (i=aEnvironment.iDlls.Size()-1;i>=0;i--)
    {
        LispString orig;
        orig = aEnvironment.iDlls[i].DllFileName();
        LispString oper;
        InternalStringify(oper, &orig);

        res = LA(ATOML(&oper[0])) + LA(res);
    }
    RESULT(aEnvironment, aStackTop).Set(LIST(LA(ATOML("List")) + LA(res)));
}



void LispDefaultTokenizer(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iDefaultTokenizer;
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}

void LispCommonLispTokenizer(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iCommonLispTokenizer;
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}
void LispCTokenizer(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iCTokenizer;
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}

void LispXmlTokenizer(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(1);
    aEnvironment.iCurrentTokenizer = &aEnvironment.iXmlTokenizer;
    LispStandard.InternalTrue(aEnvironment,RESULT(aEnvironment, aStackTop));
}

void LispExplodeTag(LispEnvironment& aEnvironment, int aStackTop)
{
    //TESTARGS(2);
    LispPtr out = new LispPtr();
    out.Set(ARGUMENT(aEnvironment, aStackTop, 1).Get());
    CHK_ISSTRING_CORE(out,1);

    LispCharPtr str = out.Get().String().String();
    str++;
    if (str[0] != '<')
    {
        RESULT(aEnvironment, aStackTop).Set(out.Get());
        return;
    }
    LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str[0] == '<',1);
    str++;
    LispCharPtr type = "\"Open\"";

    if (str[0] == '/')
    {
        type = "\"Close\"";
        str++;
    }
    LispString tag;
    tag.Resize(0);
    
    tag.Append('\"');
    while (IsAlpha(*str))
    {
        LispChar c = *str++;
        if (c >= 'a' && c <= 'z')
            c = c + ('A'-'a');
        tag.Append(c);
    }
    tag.Append('\"');
    tag.Append('\0');

    LispObject* info = null;

    while (*str == ' ') str++;
    while (*str != '>' && *str != '/')
    {
        LispString name;
        name.Resize(0);
        name.Append('\"');

        while (IsAlpha(*str))
        {
            LispChar c = *str++;
            if (c >= 'a' && c <= 'z')
                c = c + ('A'-'a');
            name.Append(c);
        }
        name.Append('\"');
        name.Append('\0');
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str[0] == '=',1);
        str++;
        LispError.CHK_ARG_CORE(aEnvironment,aStackTop,str[0] == '\"',1);
        LispString value;
        value.Resize(0);
        value.Append(*str++);
        while (*str != '\"')
        {
            value.Append(*str++);
        }
        value.Append(*str++);
        value.Append('\0');
//printf("[%s], [%s]\n",name.String(),value.String());
        info =  LIST(LA(ATOML("List")) + LA(ATOML(name.String())) + LA(ATOML(value.String()))) + LA(info);
        while (*str == ' ') str++;

//printf("End is %c\n",str[0]);
    }
    if (*str == '/')
    {
      type = "\"OpenClose\"";
      str++;
      while (*str == ' ') str++;
    }
    
    info = LIST(LA(ATOML("List")) + LA(info));
    RESULT(aEnvironment, aStackTop).Set(
                LIST(
                     LA(ATOML("XmlTag")) +
                     LA(ATOML(tag.String())) +
                     LA(info) +
                     LA(ATOML(type))
                    )
               );
}


/// convert bits to digits. Use the kernel function bits_to_digits. Arguments must be small integers.
void LispBitsToDigits(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
	  long result = 0;	// initialize just in case
	  if (x.IsInt() && x.IsSmall() && y.IsInt() && y.IsSmall())
	  {
		  // bits_to_digits uses unsigned long, see numbers.h
			unsigned base = unsigned(y.Double());
			result = bits_to_digits((unsigned long)(x.Double()), base);
	  }
	  else
	  {
		  RaiseError("BitsToDigits: error: arguments (%f, %f) must be small integers", x.Double(), y.Double());		  
	  }
      BigNumber *z = new BigNumber();
      z.SetTo((long)result);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));

#else	// this is not defined without BigNumber, so return something
    RaiseError("Function BitsToDigits is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}

/// convert digits to bits. Use the kernel function digits_to_bits. Arguments must be small integers.
void LispDigitsToBits(LispEnvironment& aEnvironment, int aStackTop)
{
#ifndef NO_USE_BIGFLOAT
      RefPtr<BigNumber> x;
      RefPtr<BigNumber> y;
      GetNumber(x,aEnvironment, aStackTop, 1);
      GetNumber(y,aEnvironment, aStackTop, 2);
	  long result = 0;	// initialize just in case
	  if (x.IsInt() && x.IsSmall() && y.IsInt() && y.IsSmall())
	  {
		  // bits_to_digits uses unsigned long, see numbers.h
			unsigned base = unsigned(y.Double());
			result = digits_to_bits((unsigned long)(x.Double()), base);
	  }
	  else
	  {
		  RaiseError("DigitsToBits: error: arguments (%f, %f) must be small integers", x.Double(), y.Double());
	  }
      BigNumber *z = new BigNumber();
      z.SetTo((long)result);
      RESULT(aEnvironment, aStackTop).Set(new LispNumber(z));

#else	// this is not defined without BigNumber, so return something
    RaiseError("Function BitsToDigits is not available without BigNumber support");
    LispArithmetic1(aEnvironment, aStackTop, FloorFloat);
#endif
}

