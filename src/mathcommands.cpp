
#include "yacasprivate.h"

#include "yacasbase.h"
#include "lispenvironment.h"
#include "standard.h"
#include "lispeval.h"
#include "lispatom.h"
#include "lispparser.h"
#include "stdfileio.h"
#include "stringio.h"
#include "lisperror.h"
#include "infixparser.h"
#include "lispuserfunc.h"
#include "mathuserfunc.h"
#include "platmath.h"
#include "numbers.h"
#include "arrayclass.h"
#include "patternclass.h"
#include "substitute.h"
#include "errors.h"
#include "arggetter.h"

#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)


void LispLexCompare2(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispBoolean (*lexfunc)(LispChar * f1, LispChar * f2, LispHashTable& aHashTable,LispInt aPrecision),
                     LispBoolean (*numfunc)(BigNumber& n1, BigNumber& n2)
                    );


void LispQuote(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    RESULT = (ARGUMENT(1)->Copy());
}

void LispEval(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
}

/// Execute the Yacas commands \c Set and \c MacroSet.
/// The argument \a aMacroMode determines whether the first argument
/// should be evaluated. The real work is done by
/// LispEnvironment::SetVariable() .
/// \sa LispSetVar(), LispMacroSetVar()
static void InternalSetVar(LispEnvironment& aEnvironment, LispInt aStackTop, LispBoolean aMacroMode, LispBoolean aGlobalLazyVariable)
{
    LispString * varstring=NULL;
    if (aMacroMode)
    {
        LispPtr result;
        InternalEval(aEnvironment, result, ARGUMENT(1));
#ifdef YACAS_DEBUG
        aEnvironment.DebugModeVerifySettingGlobalVariables(result, aGlobalLazyVariable);
#endif // YACAS_DEBUG
        varstring = result->String();
    }
    else
    {
#ifdef YACAS_DEBUG
        aEnvironment.DebugModeVerifySettingGlobalVariables(ARGUMENT(1), aGlobalLazyVariable);
#endif // YACAS_DEBUG
        varstring = ARGUMENT(1)->String();
    }
    CHK_ARG_CORE(varstring,1);
    CHK_ARG_CORE(!IsNumber(varstring->c_str(),LispTrue),1);
 
    LispPtr result;
    InternalEval(aEnvironment, result, ARGUMENT(2));
    aEnvironment.SetVariable(varstring, result, aGlobalLazyVariable);
    InternalTrue(aEnvironment,RESULT);
}

/// Corresponds to the Yacas function \c Set.
/// This function simply calls InternalSetVar() .
void LispSetVar(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  InternalSetVar(aEnvironment, aStackTop, LispFalse,LispFalse);
}

/// Corresponds to the Yacas function \c MacroSet.
/// This function simply calls InternalSetVar() .
void LispMacroSetVar(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  InternalSetVar(aEnvironment, aStackTop, LispTrue,LispFalse);
}

void LispSetGlobalLazyVariable(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  InternalSetVar(aEnvironment, aStackTop, LispFalse,LispTrue);
}


void LispClearVar(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  LispPtr* subList = ARGUMENT(1)->SubList();
  if (subList)
  {
    LispIterator iter(*subList);
    for (LispInt nr=1; (++iter).getObj(); nr++)
    {
      LispString * str = iter.getObj()->String();
      CHK_ARG_CORE(str, nr);
      aEnvironment.UnsetVariable(str);
    }
  }
  InternalTrue(aEnvironment,RESULT);
}


/* StrCompare returns f1-f2: if f1 < f2 it returns -1, if f1=f2 it
 returns 0, and it returns 1 if f1>f2
 */
// the aPrecision argument is ignored here
static LispBoolean LexLessThan(LispChar * f1, LispChar * f2, LispHashTable& aHashTable,LispInt aPrecision)
{
    return (StrCompare(f1, f2)<0);
}

// the aPrecision argument is ignored here
static LispBoolean LexGreaterThan(LispChar * f1, LispChar * f2, LispHashTable& aHashTable,LispInt aPrecision)
{
    return (StrCompare(f1, f2)>0);
}

static LispBoolean BigLessThan(BigNumber& n1, BigNumber& n2)
{
  return n1.LessThan(n2) && !n1.Equals(n2);
}
static LispBoolean BigGreaterThan(BigNumber& n1, BigNumber& n2)
{
  return !(n1.LessThan(n2) || n1.Equals(n2));
}

void LispLessThan(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispLexCompare2(aEnvironment, aStackTop, LexLessThan,BigLessThan);
}

void LispGreaterThan(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispLexCompare2(aEnvironment, aStackTop, LexGreaterThan, BigGreaterThan);
}


void LispLexCompare2(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispBoolean (*lexfunc)(LispChar * f1, LispChar * f2, LispHashTable& aHashTable,LispInt aPrecision),
                     LispBoolean (*numfunc)(BigNumber& n1, BigNumber& n2)
                    )
{
    LispPtr result1(ARGUMENT(1));
    LispPtr result2(ARGUMENT(2));
    LispBoolean cmp;
    RefPtr<BigNumber> n1; n1 = result1->Number(aEnvironment.Precision());
    RefPtr<BigNumber> n2; n2 = result2->Number(aEnvironment.Precision());
    if (!!n1 && !!n2)
    {
      cmp =numfunc(*n1,*n2);
    }
    else
    {
      LispString * str1 = result1->String();
      LispString * str2 = result2->String();
      CHK_ARG_CORE(str1 ,1);
      CHK_ARG_CORE(str2, 2);
       // the precision argument is ignored in "lex" functions
      cmp = lexfunc(str1->c_str(),str2->c_str(),
                            aEnvironment.HashTable(),
                            aEnvironment.Precision());
    }
 
    InternalBoolean(aEnvironment,RESULT, cmp);
}



void LispFullForm(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    RESULT = (ARGUMENT(1));
    LispPrinter printer;
    printer.Print(RESULT, *aEnvironment.CurrentOutput(), aEnvironment);
    aEnvironment.CurrentOutput()->Write("\n");
}


void LispHead(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  InternalNth(RESULT, ARGUMENT(1),1);
}


void LispNth(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispString * str = ARGUMENT(2)->String();
    CHK_ARG_CORE(str,2);
    CHK_ARG_CORE(IsNumber(str->c_str(),LispFalse),2);
    LispInt index = InternalAsciiToInt(str);
    InternalNth(RESULT, ARGUMENT(1), index);
}


void LispTail(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr first;
    InternalTail(first, ARGUMENT(1));
    InternalTail(RESULT, first);
    LispPtr head(aEnvironment.iList->Copy());
    head->Nixed() = ((*RESULT->SubList()));
    (*RESULT->SubList()) = (head);
}

void LispUnList(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  CHK_ARG_CORE(ARGUMENT(1), 1);
  CHK_ARG_CORE(ARGUMENT(1)->SubList(), 1);
  LispObject* subList = (*ARGUMENT(1)->SubList());
  CHK_ARG_CORE(subList, 1);
  CHK_ARG_CORE(subList->String() == aEnvironment.iList->String(),1);
  InternalTail(RESULT, ARGUMENT(1));
}

void LispListify(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  CHK_ARG_CORE(ARGUMENT(1)->SubList(), 1);
  LispPtr head(aEnvironment.iList->Copy());
  head->Nixed() = ((*ARGUMENT(1)->SubList()));
  RESULT = (LispSubList::New(head));
}




void LispDestructiveReverse(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  CHK_ISLIST_CORE(ARGUMENT(1),1);

  LispPtr reversed(aEnvironment.iList->Copy());
  InternalReverseList(reversed->Nixed(), (*ARGUMENT(1)->SubList())->Nixed());
  RESULT = (LispSubList::New(reversed));
}


void LispLength(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr* subList = ARGUMENT(1)->SubList();
  if (subList)
  {
    LispChar s[20];
    LispInt num = InternalListLength((*subList)->Nixed());
    InternalIntToAscii(s,num);
    RESULT = (LispAtom::New(aEnvironment,s));
    return;
  }
  LispString * string = ARGUMENT(1)->String();
  if (InternalIsString(string))
  {
    LispChar s[20];
    LispInt num = string->Size()-3;
    InternalIntToAscii(s,num);
    RESULT = (LispAtom::New(aEnvironment,s));
    return;
  }
  GenericClass *gen = ARGUMENT(1)->Generic();
  DYNCAST(ArrayClass,"\"Array\"",arr,gen)
  if (arr)
  {
    LispInt size = arr->Size();
    LispChar s[20];
    InternalIntToAscii(s,size);
    RESULT = (LispAtom::New(aEnvironment,s));
    return;
  }
//  CHK_ISLIST_CORE(ARGUMENT(1),1);
}

void LispList(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr all(aEnvironment.iList->Copy());
  LispIterator tail(all);
  ++tail;
  LispIterator iter(*ARGUMENT(1)->SubList());
  while ((++iter).getObj())
  {
    LispPtr evaluated;
    InternalEval(aEnvironment,evaluated,*iter);
  // Ideally this would work, but it does not yet: (*tail++) = (evaluated)
    (*tail) = (evaluated);
    ++tail;
  }
  RESULT = (LispSubList::New(all));
}


void LispConcatenate(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr all(aEnvironment.iList->Copy());
  LispIterator tail(all);
  ++tail;
  LispIterator iter(*ARGUMENT(1)->SubList());
  for (LispInt arg = 1; (++iter).getObj(); arg++)
  {
    CHK_ISLIST_CORE(*iter,arg);
    InternalFlatCopy(*tail,(*(*iter)->SubList())->Nixed());  // TODO: woof -- prefer below
    //InternalFlatCopy(*tail,iter.getObj()->Nixed());
    while (tail.getObj()) ++tail;
  }
  RESULT = (LispSubList::New(all));
}

static void ConcatenateStrings(LispStringSmartPtr& aResult, LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* aResult passed in by reference to avoid over-application of copy-constructors, smart pointer so the result
   * gets cleaned up automatically afterwards. aResult acts like a string buffer we can append substrings to.
   */
  LISPASSERT(aResult);
  aResult->ResizeTo(0);
  aResult->Append('\"');

  LispIterator iter(*ARGUMENT(1)->SubList());
  LispInt arg;
  for (arg=1; (++iter).getObj(); arg++)
  {
    CHK_ISSTRING_CORE(*iter,arg);
    LispInt length = iter.getObj()->String()->Size()-2;
    LispChar * ptr = iter.getObj()->String()->c_str();
    LispString * str = aResult;
    LispInt curlen = str->Size();
    str->ResizeTo(curlen+length-1);
    LispChar * put = &(*str)[curlen-1];
    PlatMemCopy(put+1,ptr+1,length-1);
  }
  aResult->Append('\"');
  aResult->Append('\0');
}

void LispConcatenateStrings(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispString *str = NEW LispString;
    LispStringSmartPtr smartptr;
    smartptr = (str);
    ConcatenateStrings(smartptr,aEnvironment, aStackTop);
    RESULT = (LispAtom::New(aEnvironment,str->c_str()));
}

static void InternalDelete(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aDestructive)
{
    LispPtr evaluated(ARGUMENT(1));
    CHK_ISLIST_CORE(evaluated,1);

    LispPtr copied;
    if (aDestructive)
    {
        copied = ((*evaluated->SubList()));
    }
    else
    {
        InternalFlatCopy(copied,*evaluated->SubList());
    }

    LispPtr index(ARGUMENT(2));
    CHK_ARG_CORE(index, 2);
    CHK_ARG_CORE(index->String(), 2);
    LispInt ind = InternalAsciiToInt(index->String());
    CHK_ARG_CORE(ind>0,2);

    LispIterator iter(copied);
  while (--ind>=0) ++iter;
  CHK_CORE(iter.getObj(), KLispErrListNotLongEnough);
  LispIterator temp = iter++;
    (*temp) = (*iter);
    RESULT = (LispSubList::New(copied));
}

void LispDelete(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalDelete(aEnvironment, aStackTop,LispFalse);
}

void LispDestructiveDelete(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalDelete(aEnvironment, aStackTop,LispTrue);
}

void LispFlatCopy(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr copied;
    if (ARGUMENT(1)->SubList() == NULL)
    {
      CHK_ISLIST_CORE(ARGUMENT(1),1);
    }
    InternalFlatCopy(copied,*ARGUMENT(1)->SubList());
    RESULT = (LispSubList::New(copied));
}

static void InternalInsert(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aDestructive)
{
    LispPtr evaluated(ARGUMENT(1));
    CHK_ISLIST_CORE(evaluated,1);

    LispPtr copied;
    if (aDestructive)
    {
        copied = ((*evaluated->SubList()));
    }
    else
    {
        InternalFlatCopy(copied,*evaluated->SubList());
    }
 
    LispPtr index(ARGUMENT(2));
    CHK_ARG_CORE(index, 2);
    CHK_ARG_CORE(index->String(), 2);
    LispInt ind = InternalAsciiToInt(index->String());
    CHK_ARG_CORE(ind>0,2);

    LispIterator iter(copied);
    while (--ind>=0) ++iter;
    LispPtr toInsert(ARGUMENT(3));
    toInsert->Nixed() = (iter.getObj());
    (*iter) = (toInsert);
    RESULT = (LispSubList::New(copied));
}

void LispInsert(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalInsert(aEnvironment, aStackTop,LispFalse);
}

void LispDestructiveInsert(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalInsert(aEnvironment, aStackTop,LispTrue);
}

static void InternalReplace(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aDestructive)
{
    LispPtr evaluated(ARGUMENT(1));
//    CHK_ISLIST_CORE(evaluated,1);
    // Ok, so lets not check if it is a list, but it needs to be at least a 'function'
    CHK_ARG_CORE(evaluated->SubList(), 1);

    LispPtr index(ARGUMENT(2));
    CHK_ARG_CORE(index, 2);
    CHK_ARG_CORE(index->String(), 2);
    LispInt ind = InternalAsciiToInt(index->String());

    LispPtr copied;
    if (aDestructive)
    {
        copied = ((*evaluated->SubList()));
    }
    else
    {
        InternalFlatCopy(copied,*evaluated->SubList());
    }
    CHK_ARG_CORE(ind>0,2);

    LispIterator iter(copied);
  while (--ind>=0) ++iter;
    LispPtr toInsert(ARGUMENT(3));
    CHK_ARG_CORE(iter.getObj(), 2);
  LispIterator temp = iter++;
    toInsert->Nixed() = (*iter);
    (*temp) = (toInsert);
    RESULT = (LispSubList::New(copied));
}

void LispReplace(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalReplace(aEnvironment, aStackTop,LispFalse);
}

void LispDestructiveReplace(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalReplace(aEnvironment, aStackTop,LispTrue);
}

void LispNot(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));
    if (IsTrue(aEnvironment, evaluated) || IsFalse(aEnvironment, evaluated))
    {
        InternalNot(RESULT, aEnvironment, evaluated);
    }
    else
    {
        LispPtr ptr(ARGUMENT(0)->Copy());
        ptr->Nixed() = (evaluated);
        RESULT = (LispSubList::New(ptr));
    }
}

void LispLazyAnd(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr nogos;
    LispInt nrnogos = 0;
    LispPtr evaluated;

    LispIterator iter(*ARGUMENT(1)->SubList());
  while ((++iter).getObj())
    {
        InternalEval(aEnvironment, evaluated, *iter);
        if (IsFalse(aEnvironment, evaluated))
        {
            InternalFalse(aEnvironment,RESULT);
            return;
        }
        else if (!IsTrue(aEnvironment, evaluated))
        {
            nrnogos++;
            LispPtr ptr(evaluated->Copy());
            ptr->Nixed() = (nogos);
            nogos = (ptr);
        }
    }

  if (!!nogos)
    {
        if (nrnogos == 1)
        {
            RESULT = (nogos);
        }
        else
        {
            LispPtr ptr;
            InternalReverseList(ptr, nogos);
            nogos = (ptr);

            ptr = (ARGUMENT(0)->Copy());
            ptr->Nixed() = (nogos);
            nogos = (ptr);
            RESULT = (LispSubList::New(nogos));
            //aEnvironment.CurrentPrinter().Print(RESULT, *aEnvironment.CurrentOutput());
        }
    }
    else
    {
        InternalTrue(aEnvironment,RESULT);
    }
}

void LispLazyOr(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr nogos;
    LispInt nrnogos = 0;

    LispPtr evaluated;

    LispIterator iter(*ARGUMENT(1)->SubList());
    while ((++iter).getObj())
    {
        InternalEval(aEnvironment, evaluated, *iter);
        if (IsTrue(aEnvironment, evaluated))
        {
            InternalTrue(aEnvironment,RESULT);
            return;
        }
        else if (!IsFalse(aEnvironment, evaluated))
        {
            nrnogos++;
            LispPtr ptr(evaluated->Copy());
            ptr->Nixed() = (nogos);
            nogos = (ptr);
        }
    }

  if (!!nogos)
    {
        if (nrnogos == 1)
        {
            RESULT = (nogos);
        }
        else
        {
            LispPtr ptr;
            InternalReverseList(ptr, nogos);
            nogos = (ptr);

            ptr = (ARGUMENT(0)->Copy());
            ptr->Nixed() = (nogos);
            nogos = (ptr);
            RESULT = (LispSubList::New(nogos));
        }
        //aEnvironment.CurrentPrinter().Print(RESULT, *aEnvironment.CurrentOutput());
    }
    else
    {
        InternalFalse(aEnvironment,RESULT);
    }
}

void LispEquals(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr evaluated1(ARGUMENT(1));
    LispPtr evaluated2(ARGUMENT(2));

    InternalBoolean(aEnvironment,RESULT,
                    InternalEquals(aEnvironment, evaluated1, evaluated2));
}

void LispWrite(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr* subList = ARGUMENT(1)->SubList();
  if (subList)
  {
    LispIterator iter(*subList);
    while ((++iter).getObj())
    {
      aEnvironment.CurrentPrinter().Print(*iter,*aEnvironment.CurrentOutput(),aEnvironment);
    }
  }
  InternalTrue(aEnvironment,RESULT);
}

void LispWriteString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  CHK_ARG_CORE(ARGUMENT(1),1);
  LispString * str = ARGUMENT(1)->String();
  CHK_ARG_CORE(str,1);
  CHK_ARG_CORE((*str)[0] == '\"',1);
  CHK_ARG_CORE((*str)[str->Size()-2] == '\"',1);

  LispInt i=1;
  LispInt nr=str->Size()-2;
  //((*str)[i] != '\"')
  for (i=1;i<nr;i++)
  {
    aEnvironment.CurrentOutput()->PutChar((*str)[i]);
  }
  // pass last printed character to the current printer
  aEnvironment.CurrentPrinter().RememberLastChar((*str)[nr-1]);  // hacky hacky
  InternalTrue(aEnvironment,RESULT);
}

void LispProgBody(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  // Allow accessing previous locals.
  LispLocalFrame frame(aEnvironment,LispFalse);

  InternalTrue(aEnvironment,RESULT);
 
  // Evaluate args one by one.

  LispIterator iter(*ARGUMENT(1)->SubList());
  while ((++iter).getObj())
  {
    InternalEval(aEnvironment, RESULT, *iter);
  }
}

void LispNewLocal(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr* subList = ARGUMENT(1)->SubList();
  if (subList)
  {
    LispIterator iter(*subList);
    for (LispInt nr = 1; (++iter).getObj(); nr++)
    {
      LispString * variable = iter.getObj()->String();
      CHK_ARG_CORE(variable,nr);
// printf("Variable %s\n",variable->String());
      aEnvironment.NewLocal(variable,NULL);
    }
  }
  InternalTrue(aEnvironment,RESULT);
}

void LispWhile(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr& arg1 = ARGUMENT(1);
    LispPtr& arg2 = ARGUMENT(2);
 
    LispPtr predicate;
    InternalEval(aEnvironment, predicate, arg1);

    while (IsTrue(aEnvironment,predicate))
    {
        LispPtr evaluated;
        InternalEval(aEnvironment, evaluated, arg2);
        InternalEval(aEnvironment, predicate, arg1);
    }
    CHK_ARG_CORE(IsFalse(aEnvironment,predicate),1);
    InternalTrue(aEnvironment,RESULT);
}

static void MultiFix(LispEnvironment& aEnvironment, LispInt aStackTop, LispOperators& aOps)
{

    // Get operator
    CHK_ARG_CORE(ARGUMENT(1), 1);
    LispString * orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);
 
    LispPtr precedence;
    InternalEval(aEnvironment, precedence, ARGUMENT(2));
    CHK_ARG_CORE(precedence->String(), 2);
    LispInt prec = InternalAsciiToInt(precedence->String());
    CHK_ARG_CORE(prec <= KMaxPrecedence, 2);
    aOps.SetOperator(prec,SymbolName(aEnvironment,orig->c_str()));
    InternalTrue(aEnvironment,RESULT);
}

void LispInFix(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    MultiFix(aEnvironment, aStackTop, aEnvironment.InFix());
}

static void SingleFix(LispInt aPrecedence, LispEnvironment& aEnvironment, LispInt aStackTop, LispOperators& aOps)
{
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1), 1);
    LispString * orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);
    aOps.SetOperator(aPrecedence,SymbolName(aEnvironment,orig->c_str()));
    InternalTrue(aEnvironment,RESULT);
}

void LispPreFix(LispEnvironment& aEnvironment, LispInt aStackTop)
{
/*
    LispInt nrArguments = InternalListLength(ARGUMENT(0));
    if (nrArguments == 2)
    {
        SingleFix(0, aEnvironment, aStackTop, aEnvironment.PreFix());
    }
    else
*/
    {
        MultiFix(aEnvironment, aStackTop, aEnvironment.PreFix());
    }
}

void LispPostFix(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInt nrArguments = InternalListLength(ARGUMENT(0));
    if (nrArguments == 2)
    {
        SingleFix(0, aEnvironment, aStackTop, aEnvironment.PostFix());
    }
    else
    {
        MultiFix(aEnvironment, aStackTop, aEnvironment.PostFix());
    }
//    SingleFix(0, aEnvironment, RESULT,aArguments, aEnvironment.PostFix());
}

void LispBodied(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    MultiFix(aEnvironment, aStackTop, aEnvironment.Bodied());
}

void LispAtomize(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    // Get operator
    CHK_ARG_CORE(evaluated, 1);
    LispString * orig = evaluated->String();
    CHK_ARG_CORE(orig, 1);
    RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpUnStringify(orig->c_str())->c_str()));
}

void LispStringify(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    // Get operator
    CHK_ARG_CORE(evaluated, 1);
    LispString * orig = evaluated->String();
    CHK_ARG_CORE(orig, 1);

    RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(orig->c_str())->c_str()));
}

void LispLoad(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated(ARGUMENT(1));

    // Get file name
    CHK_ARG_CORE(evaluated, 1);
    LispString * orig = evaluated->String();
    CHK_ARG_CORE(orig, 1);

    InternalLoad(aEnvironment,orig);
    InternalTrue(aEnvironment,RESULT);
}

/// Implements the Yacas functions \c RuleBase and \c MacroRuleBase .
/// The real work is done by LispEnvironment::DeclareRuleBase().
static void InternalRuleBase(LispEnvironment& aEnvironment, LispInt aStackTop,
                             LispInt aListed)
{
    // Get operator
 
    CHK_ARG_CORE(ARGUMENT(1), 1);
    LispString * orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);
    LispPtr args(ARGUMENT(2));
 
    // The arguments
    CHK_ISLIST_CORE(args,2);

    // Finally define the rule base
    aEnvironment.DeclareRuleBase(SymbolName(aEnvironment,orig->c_str()),
                                 (*args->SubList())->Nixed(),aListed);
 
    // Return LispTrue
    InternalTrue(aEnvironment,RESULT);
}

/// Corresponds to the Yacas function \c RuleBase .
/// This function simply calls InternalRuleBase().
void LispRuleBase(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalRuleBase(aEnvironment, aStackTop, LispFalse);
}

void LispMacroRuleBase(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalRuleBase(aEnvironment, aStackTop, LispFalse);
}

void InternalDefMacroRuleBase(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aListed)
{
    // Get operator
    //LispPtr body;
 
    CHK_ARG_CORE(ARGUMENT(1), 1);
    LispString * orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);

    // The arguments
    LispPtr args(ARGUMENT(2));
    CHK_ISLIST_CORE(args,2);

    // Finally define the rule base
    aEnvironment.DeclareMacroRuleBase(SymbolName(aEnvironment,orig->c_str()),
                                 (*args->SubList())->Nixed(),aListed);
 
    // Return LispTrue
    InternalTrue(aEnvironment,RESULT);
}

void LispDefMacroRuleBaseListed(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  InternalDefMacroRuleBase(aEnvironment, aStackTop, LispTrue);
}

void LispDefMacroRuleBase(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  InternalDefMacroRuleBase(aEnvironment, aStackTop, LispFalse);
}

void LispRuleBaseListed(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalRuleBase(aEnvironment, aStackTop, LispTrue);
}

void LispMacroRuleBaseListed(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalRuleBase(aEnvironment, aStackTop, LispTrue);
}

void LispHoldArg(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1), 1);
    LispString * orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);

    // The arguments
    LispString * tohold = ARGUMENT(2)->String();
    CHK_ARG_CORE(tohold, 2);
    aEnvironment.HoldArgument(SymbolName(aEnvironment,orig->c_str()), tohold);
    // Return LispTrue
    InternalTrue(aEnvironment,RESULT);
}

static void InternalNewRule(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInt arity;
    LispInt precedence;

    LispPtr ar;
    LispPtr pr;
    LispPtr predicate;
    LispPtr body;
    LispString * orig=NULL;
 
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1), 1);
    orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);
    ar = (ARGUMENT(2));
    pr = (ARGUMENT(3));
    predicate = (ARGUMENT(4));
    body = (ARGUMENT(5));
 
    // The arity
    CHK_ARG_CORE(ar, 2);
    CHK_ARG_CORE(ar->String(), 2);
    arity = InternalAsciiToInt(ar->String());

    // The precedence
    CHK_ARG_CORE(pr, 3);
    CHK_ARG_CORE(pr->String(), 3);
    precedence = InternalAsciiToInt(pr->String());
 
    // Finally define the rule base
    aEnvironment.DefineRule(SymbolName(aEnvironment,orig->c_str()),
                            arity,
                            precedence,
                            predicate,
                            body );

    // Return LispTrue
    InternalTrue(aEnvironment,RESULT);
}

void LispNewRule(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalNewRule(aEnvironment, aStackTop);
}

void LispMacroNewRule(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalNewRule(aEnvironment, aStackTop);
}

void LispUnFence(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1), 1);
    LispString * orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);

    // The arity
    CHK_ARG_CORE(ARGUMENT(2), 2);
    CHK_ARG_CORE(ARGUMENT(2)->String(), 2);
    LispInt arity = InternalAsciiToInt(ARGUMENT(2)->String());

    aEnvironment.UnFenceRule(SymbolName(aEnvironment,orig->c_str()),
                            arity);
 
    // Return LispTrue
    InternalTrue(aEnvironment,RESULT);
}

void LispIsFunction(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr result(ARGUMENT(1));
    InternalBoolean(aEnvironment,RESULT,
                    result->SubList()!=NULL);
}

void LispIsAtom(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr result(ARGUMENT(1));
    InternalBoolean(aEnvironment,RESULT,
                    result->String()!=NULL);
}

void LispIsNumber(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  LispPtr result(ARGUMENT(1));
  InternalBoolean(aEnvironment, RESULT, result->Number(aEnvironment.Precision()) != NULL);
}

void LispIsInteger(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  LispPtr result(ARGUMENT(1));

  RefPtr<BigNumber> num ; num = result->Number(aEnvironment.Precision());
  if (!num)
  {
    InternalFalse(aEnvironment,RESULT);
  }
  else
  {
    InternalBoolean(aEnvironment, RESULT, num->IsInt());
  }
}

void LispIsList(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr result(ARGUMENT(1));
    InternalBoolean(aEnvironment,RESULT,InternalIsList(result));
}

void LispIsString(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr result(ARGUMENT(1));
    InternalBoolean(aEnvironment,RESULT,
                    InternalIsString(result->String()));
}

void LispIsBound(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispString * str = ARGUMENT(1)->String();
    if (str)
    {
        LispPtr val;
        aEnvironment.GetVariable(str,val);
        if (!!val)
        {
            InternalTrue(aEnvironment,RESULT);
            return;
        }
    }
    InternalFalse(aEnvironment,RESULT);
}

void LispIf(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInt nrArguments = InternalListLength(ARGUMENT(0));
    CHK_CORE(nrArguments == 3 || nrArguments == 4,KLispErrWrongNumberOfArgs);

    LispPtr predicate;
    InternalEval(aEnvironment, predicate, ARGUMENT(1));

    if (IsTrue(aEnvironment,predicate))
    {
        InternalEval(aEnvironment, RESULT, Argument(ARGUMENT(0),2));
    }
    else
    {
        CHK_ARG_CORE(IsFalse(aEnvironment,predicate),1);
        if (nrArguments == 4)
        {
            InternalEval(aEnvironment, RESULT, Argument(ARGUMENT(0),3));
        }
        else
        {
            InternalFalse(aEnvironment,RESULT);
        }
    }
}

void LispRetract(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Get operator
    LispPtr evaluated(ARGUMENT(1));

    CHK_ARG_CORE(evaluated, 1);
    LispString * orig = evaluated->String();
    CHK_ARG_CORE(orig, 1);
    LispString * oper = SymbolName(aEnvironment,orig->c_str());
 
    LispPtr arity(ARGUMENT(2));
    CHK_ARG_CORE(arity->String(), 2);
    LispInt ar = InternalAsciiToInt(arity->String());
    aEnvironment.Retract(oper, ar);
    InternalTrue(aEnvironment,RESULT);
}

void YacasBuiltinPrecisionSet(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr index(ARGUMENT(1));
    CHK_ARG_CORE(index, 1);
    CHK_ARG_CORE(index->String(), 1);

    LispInt ind = InternalAsciiToInt(index->String());
    CHK_ARG_CORE(ind>0,1);
    aEnvironment.SetPrecision(ind);
    InternalTrue(aEnvironment,RESULT);
}

void LispDefaultDirectory(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Get file name
    CHK_ARG_CORE(ARGUMENT(1), 1);
    LispString * orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);
    LispString oper;
    InternalUnstringify(oper, orig);
    aEnvironment.iInputDirectories.Append(NEW LispString(oper.c_str()));
    InternalTrue(aEnvironment,RESULT);
}

void LispFromFile(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);
  LispPtr evaluated;
  InternalEval(aEnvironment, evaluated, ARGUMENT(1));

  // Get file name
  CHK_ARG_CORE(evaluated, 1);
  LispString * orig = evaluated->String();
  CHK_ARG_CORE(orig, 1);

  LispString * contents = aEnvironment.FindCachedFile(orig->c_str());
  LispString * hashedname = aEnvironment.HashTable().LookUpUnStringify(orig->c_str());

  InputStatus oldstatus = aEnvironment.iInputStatus;
  aEnvironment.iInputStatus.SetTo(hashedname->c_str());

  if (contents)
  {
    StringInput newInput(*contents,aEnvironment.iInputStatus);
    LispLocalInput localInput(aEnvironment, &newInput);

    // Evaluate the body
    InternalEval(aEnvironment, RESULT, ARGUMENT(2));
    delete contents;
  }
  else
  {
    // Open file
    LispLocalFile localFP(aEnvironment, hashedname->c_str(),LispTrue,
                          aEnvironment.iInputDirectories);
    CHK_CORE(localFP.iOpened != 0, KLispErrFileNotFound);
    FILEINPUT newInput(localFP,aEnvironment.iInputStatus);
    LispLocalInput localInput(aEnvironment, &newInput);

    // Evaluate the body
    InternalEval(aEnvironment, RESULT, ARGUMENT(2));
  }
  aEnvironment.iInputStatus.RestoreFrom(oldstatus);
  //Return the result
}

void LispFromString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr evaluated;
  InternalEval(aEnvironment, evaluated, ARGUMENT(1));

  // Get file name
  CHK_ARG_CORE(evaluated, 1);
  LispString * orig = evaluated->String();
  CHK_ARG_CORE(orig, 1);
  LispString oper;
  InternalUnstringify(oper, orig);

  InputStatus oldstatus = aEnvironment.iInputStatus;
  aEnvironment.iInputStatus.SetTo("String");
  StringInput newInput(oper,aEnvironment.iInputStatus);
  LispLocalInput localInput(aEnvironment, &newInput);

  // Evaluate the body
  InternalEval(aEnvironment, RESULT, ARGUMENT(2));
  aEnvironment.iInputStatus.RestoreFrom(oldstatus);

  //Return the result
}

void LispRead(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
  InfixParser parser(tok,
                     *aEnvironment.CurrentInput(),
                     aEnvironment,
                     aEnvironment.PreFix(),
                     aEnvironment.InFix(),
                     aEnvironment.PostFix(),
                     aEnvironment.Bodied());
  // Read expression
  parser.Parse(RESULT);
}

void LispReadToken(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
  LispString * result = tok.NextToken(*aEnvironment.CurrentInput(),
                         aEnvironment.HashTable());

  if (result->c_str()[0] == '\0')
  {
    RESULT = (aEnvironment.iEndOfFile->Copy());
    return;
  }
  RESULT = (LispAtom::New(aEnvironment,result->c_str()));
}

void LispToFile(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

  LispPtr evaluated;
  InternalEval(aEnvironment, evaluated, ARGUMENT(1));

  // Get file name
  CHK_ARG_CORE(evaluated, 1);
  LispString * orig = evaluated->String();
  CHK_ARG_CORE(orig, 1);
  LispString oper;
  InternalUnstringify(oper, orig);

  // Open file for writing
  LispLocalFile localFP(aEnvironment, oper.c_str(),LispFalse, aEnvironment.iInputDirectories);
  CHK_CORE(localFP.iOpened != 0, KLispErrFileNotFound);
  StdFileOutput newOutput(localFP);
  LispLocalOutput localOutput(aEnvironment, &newOutput);

  // Evaluate the body
  InternalEval(aEnvironment, RESULT, ARGUMENT(2));

  //Return the result
}

void LispCheck(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  LispPtr pred;
  InternalEval(aEnvironment, pred, ARGUMENT(1));
  if (!IsTrue(aEnvironment,pred))
  {
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, ARGUMENT(2));
    CHK_ISSTRING_CORE(evaluated,2);
    aEnvironment.SetUserError(evaluated->String()->c_str());
    CHK_CORE(0,KLispErrUser);
  }
  RESULT = (pred);
}

void LispTrapError(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  LispTrap(
  {
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
  },aEnvironment.iErrorOutput,aEnvironment);

  if (aEnvironment.iError[0])
  {
    InternalEval(aEnvironment, RESULT, ARGUMENT(2));
    aEnvironment.iError.ResizeTo(1);
    aEnvironment.iError[0]='\0';
  }
}

void LispGetCoreError(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(aEnvironment.iError.c_str())->c_str()));
}

void LispSystemCall(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

  LispPtr result(ARGUMENT(1));
  CHK_ISSTRING_CORE(result,1);

  LispString command;
  InternalUnstringify(command, result->String());

// we would like to pass the exit code back to Yacas. Right now, let's pass True/False according to whether the exit code is 0 or not.
#ifdef SystemCall
  InternalBoolean(aEnvironment, RESULT, SystemCall(command.c_str()) == 0);
#else
  InternalFalse(aEnvironment,RESULT);
#endif
}

void LispMaxEvalDepth(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr index(ARGUMENT(1));
    CHK_ARG_CORE(index, 1);
    CHK_ARG_CORE(index->String(), 1);

    LispInt ind = InternalAsciiToInt(index->String());
    aEnvironment.iMaxEvalDepth = ind;
    InternalTrue(aEnvironment,RESULT);
}

void LispDefLoad(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated(ARGUMENT(1));

    // Get file name
    CHK_ARG_CORE(evaluated, 1);
    LispString * orig = evaluated->String();
    CHK_ARG_CORE(orig, 1);

    LoadDefFile(aEnvironment, orig);
    InternalTrue(aEnvironment,RESULT);
}

void LispUse(LispEnvironment& aEnvironment, LispInt aStackTop)
{
//This one seems safe...    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated(ARGUMENT(1));

    // Get file name
    CHK_ARG_CORE(evaluated, 1);
    LispString * orig = evaluated->String();
    CHK_ARG_CORE(orig, 1);

    InternalUse(aEnvironment,orig);
    InternalTrue(aEnvironment,RESULT);
}

void LispRightAssociative(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1), 1);
    LispString * orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);
    aEnvironment.InFix().SetRightAssociative(SymbolName(aEnvironment,orig->c_str()));
    InternalTrue(aEnvironment,RESULT);
}

void LispLeftPrecedence(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1), 1);
    LispString * orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);

    LispPtr index;
    InternalEval(aEnvironment, index, ARGUMENT(2));
    CHK_ARG_CORE(index, 2);
    CHK_ARG_CORE(index->String(), 2);
    LispInt ind = InternalAsciiToInt(index->String());

    aEnvironment.InFix().SetLeftPrecedence(SymbolName(aEnvironment,orig->c_str()),ind);
    InternalTrue(aEnvironment,RESULT);
}

void LispRightPrecedence(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1), 1);
    LispString * orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);

    LispPtr index;
    InternalEval(aEnvironment, index, ARGUMENT(2));
    CHK_ARG_CORE(index, 2);
    CHK_ARG_CORE(index->String(), 2);
    LispInt ind = InternalAsciiToInt(index->String());

    aEnvironment.InFix().SetRightPrecedence(SymbolName(aEnvironment,orig->c_str()),ind);
    InternalTrue(aEnvironment,RESULT);
}

static LispInFixOperator* OperatorInfo(LispEnvironment& aEnvironment,LispInt aStackTop, LispOperators& aOperators)
{
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1), 1);

    LispPtr evaluated(ARGUMENT(1));

    LispString * orig = evaluated->String();
    CHK_ARG_CORE(orig, 1);

    //
    LispInFixOperator* op = aOperators.LookUp(
                                              SymbolName(aEnvironment,orig->c_str()));
    return op;
}

void LispIsInFix(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispInFixOperator* op = OperatorInfo(aEnvironment,
                                       aStackTop,
                                       aEnvironment.InFix());
  InternalBoolean(aEnvironment, RESULT, op != NULL);
}

void LispIsBodied(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispInFixOperator* op = OperatorInfo(aEnvironment,
                                       aStackTop,
                                       aEnvironment.Bodied());
  InternalBoolean(aEnvironment, RESULT, op != NULL);
}

void LispGetPrecedence(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispInFixOperator* op = OperatorInfo(aEnvironment,
                                       aStackTop,
                                       aEnvironment.InFix());
  if (!op)
  {  // also need to check for a postfix or prefix operator
    op = OperatorInfo(aEnvironment,
                      aStackTop,
                      aEnvironment.PreFix());
    if (!op)
    {
      op = OperatorInfo(aEnvironment,
                        aStackTop,
                        aEnvironment.PostFix());
      if (!op)
      {  // or maybe it's a bodied function
        op = OperatorInfo(aEnvironment,
                          aStackTop,
                          aEnvironment.Bodied());
        CHK_CORE(op!=NULL, KLispErrIsNotInFix);
      }
    }
  }
  LispChar buf[30];
  InternalIntToAscii(buf, op->iPrecedence);
  RESULT = (LispAtom::New(aEnvironment,buf));
}

void LispGetLeftPrecedence(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.InFix());
    if (!op) {  // infix and postfix operators have left precedence
      op = OperatorInfo(aEnvironment,
                          aStackTop,
                          aEnvironment.PostFix());
        CHK_CORE(op!=NULL, KLispErrIsNotInFix);
  }

    LispChar buf[30];
    InternalIntToAscii(buf, op->iLeftPrecedence);
    RESULT = (LispAtom::New(aEnvironment,buf));
}

void LispGetRightPrecedence(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.InFix());
    if (!op) {   // bodied, infix and prefix operators have right precedence
        op = OperatorInfo(aEnvironment,
                          aStackTop,
                          aEnvironment.PreFix());
        if (!op) {   // or maybe it's a bodied function
            op = OperatorInfo(aEnvironment,
                          aStackTop,
                          aEnvironment.Bodied());
            CHK_CORE(op!=NULL, KLispErrIsNotInFix);
        }
    }

    LispChar buf[30];
    InternalIntToAscii(buf, op->iRightPrecedence);
    RESULT = (LispAtom::New(aEnvironment,buf));
}

void LispIsPreFix(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.PreFix());
  InternalBoolean(aEnvironment, RESULT, op != NULL);
}

void LispIsPostFix(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispInFixOperator* op = OperatorInfo(aEnvironment,
                                       aStackTop,
                                       aEnvironment.PostFix());

  InternalBoolean(aEnvironment, RESULT, op != NULL);
}

void YacasBuiltinPrecisionGet(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispChar buf[30];
  // decimal precision
    InternalIntToAscii(buf, aEnvironment.Precision());
    RESULT = (LispAtom::New(aEnvironment,buf));
}

void LispToString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispString oper;
    StringOutput newOutput(oper);

    LispLocalOutput localOutput(aEnvironment, &newOutput);

    // Evaluate the body
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));

    //Return the result
    RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(oper.c_str())->c_str()));
}

void LispToStdout(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispLocalOutput localOutput(aEnvironment, aEnvironment.iInitialOutput);
    // Evaluate the body
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
}

void LispSecure(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispSecureFrame security(aEnvironment);
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
}

void LispFindFile(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);
 
    LispPtr evaluated(ARGUMENT(1));

    // Get file name
    CHK_ARG_CORE(evaluated, 1);
    LispString * orig = evaluated->String();
    CHK_ARG_CORE(orig, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispChar filename[1024];//TODO FIXME
    InternalFindFile(oper.c_str(), aEnvironment.iInputDirectories,
                     filename);
    LispString res(filename,1);
    RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(res.c_str())->c_str()));
}

void LispIsGeneric(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  LispPtr evaluated(ARGUMENT(1));

  InternalBoolean(aEnvironment, RESULT, evaluated->Generic() != NULL);
}

void LispGenericTypeName(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    CHK_ARG_CORE(evaluated->Generic(),1);

    LispChar * name = evaluated->Generic()->TypeName();
    RESULT = (LispAtom::New(aEnvironment,name));
}

void GenArrayCreate(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr sizearg(ARGUMENT(1));

    CHK_ARG_CORE(sizearg, 1);
    CHK_ARG_CORE(sizearg->String(), 1);

    LispInt size = InternalAsciiToInt(sizearg->String());

    LispPtr initarg(ARGUMENT(2));
 
    ArrayClass *array = NEW ArrayClass(size,initarg);
    RESULT = (LispGenericClass::New(array));
}

void GenArraySize(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    GenericClass *gen = evaluated->Generic();
    DYNCAST(ArrayClass,"\"Array\"",arr,gen)
    CHK_ARG_CORE(arr,1);
    LispInt size = arr->Size();
    LispChar s[20];
    InternalIntToAscii(s,size);
    RESULT = (LispAtom::New(aEnvironment,s));
}

void GenArrayGet(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    GenericClass *gen = evaluated->Generic();
    DYNCAST(ArrayClass,"\"Array\"",arr,gen)
    CHK_ARG_CORE(arr,1);
    LispPtr sizearg(ARGUMENT(2));

    CHK_ARG_CORE(sizearg, 2);
    CHK_ARG_CORE(sizearg->String(), 2);

    LispInt size = InternalAsciiToInt(sizearg->String());

    CHK_ARG_CORE(size>0 && size<=arr->Size(),2);
    LispObject* object = arr->GetElement(size);
    RESULT = (object->Copy());
}

void GenArraySet(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  LispPtr evaluated(ARGUMENT(1));

  GenericClass *gen = evaluated->Generic();
  DYNCAST(ArrayClass,"\"Array\"",arr,gen)
  CHK_ARG_CORE(arr,1);
  LispPtr sizearg(ARGUMENT(2));

  CHK_ARG_CORE(sizearg, 2);
  CHK_ARG_CORE(sizearg->String(), 2);

  LispInt size = InternalAsciiToInt(sizearg->String());

  CHK_ARG_CORE(size>0 && size<=arr->Size(),2);
  LispPtr obj(ARGUMENT(3));
  arr->SetElement(size,obj);

  InternalTrue( aEnvironment, RESULT);
}

void LispCustomEval(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  if (aEnvironment.iDebugger) delete aEnvironment.iDebugger;
  aEnvironment.iDebugger = NEW DefaultDebugger(ARGUMENT(1), ARGUMENT(2),ARGUMENT(3));
  LispLocalEvaluator local(aEnvironment,NEW TracedEvaluator);
  aEnvironment.iDebugger->Start();
  InternalEval(aEnvironment, RESULT, ARGUMENT(4));
  aEnvironment.iDebugger->Finish();
  delete aEnvironment.iDebugger;
  aEnvironment.iDebugger = NULL;
}

void LispCustomEvalExpression(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  if (!aEnvironment.iDebugger)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  RESULT = (aEnvironment.iDebugger->iTopExpr);
}

void LispCustomEvalResult(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  if (!aEnvironment.iDebugger)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  RESULT = (aEnvironment.iDebugger->iTopResult);
}

void LispCustomEvalLocals(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  aEnvironment.CurrentLocals(RESULT);
}

void LispCustomEvalStop(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  if (!aEnvironment.iDebugger)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  aEnvironment.iDebugger->iStopped = LispTrue;

  InternalTrue(aEnvironment,RESULT);
}

void LispTraceStack(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispLocalEvaluator local(aEnvironment,NEW TracedStackEvaluator);
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
}

void LispReadLisp(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
    LispParser parser(tok,
                      *aEnvironment.CurrentInput(),
                      aEnvironment);
    // Read expression
    parser.Parse(RESULT);
}

void LispReadLispListed(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
    LispParser parser(tok,
                      *aEnvironment.CurrentInput(),
                      aEnvironment);
    parser.iListed = LispTrue;
    // Read expression
    parser.Parse(RESULT);
}

void LispTraceRule(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr *ptr = ARGUMENT(0)->Nixed()->SubList();
    LispUserFunction* userfunc=NULL;
    if (ptr)
        userfunc = GetUserFunction(aEnvironment,ptr);
    LispLocalTrace trace(userfunc);
    InternalEval(aEnvironment, RESULT, ARGUMENT(2));
}

void LispType(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));
    LispPtr* subList = evaluated->SubList();
    LispObject* head = NULL;
    if (!subList)
    {
        goto EMPTY;
    }
    head = (*subList);
    if (!head->String())
        goto EMPTY;
    RESULT = (LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(head->String()->c_str())->c_str()));
    return;
 
EMPTY:
    RESULT = (LispAtom::New(aEnvironment,"\"\""));
    return;
}

void YacasStringMidGet(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr evaluated(ARGUMENT(3));
    CHK_ISSTRING_CORE(evaluated,3);
    LispString * orig = evaluated->String();
 
    LispPtr index(ARGUMENT(1));
    CHK_ARG_CORE(index, 1);
    CHK_ARG_CORE(index->String(), 1);
    LispInt from = InternalAsciiToInt(index->String());
    CHK_ARG_CORE(from>0,1);
 
    index = (ARGUMENT(2));
    CHK_ARG_CORE(index, 2);
    CHK_ARG_CORE(index->String(), 2);
    LispInt count = InternalAsciiToInt(index->String());
 
    LispString str;
    str.ResizeTo(0);
    str.Append('\"');
    LispInt i;
    CHK_CORE(from+count<orig->Size()-1, KLispErrInvalidArg);
    for (i=from;i<from+count;i++)
        str.Append((*orig)[i]);
    str.Append('\"');
    str.Append('\0');
    RESULT = (LispAtom::New(aEnvironment,str.c_str()));
}

void YacasStringMidSet(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr evaluated(ARGUMENT(3));
    CHK_ISSTRING_CORE(evaluated,3);
    LispString * orig = evaluated->String();
    LispPtr index(ARGUMENT(1));
    CHK_ARG_CORE(index, 1);
    CHK_ARG_CORE(index->String(), 1);
    LispInt from = InternalAsciiToInt(index->String());

    CHK_ARG_CORE(from>0,1);
 
    LispPtr ev2(ARGUMENT(2));
    CHK_ISSTRING_CORE(ev2,2);
    LispString * replace = ev2->String();

    LispString str(orig->c_str());
    LispInt i;
    LispInt count = replace->Size();
    CHK_CORE(from+count-3<orig->Size()-1, KLispErrInvalidArg);

    for (i=0;i<count-3;i++)
        str[i+from] = (*replace)[i+1];
    RESULT = (LispAtom::New(aEnvironment,str.c_str()));
}

void LispFindFunction(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);
 
    LispPtr evaluated(ARGUMENT(1));

    // Get file name
    CHK_ARG_CORE(evaluated, 1);
    LispString * orig = evaluated->String();
    CHK_ARG_CORE(orig, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper.c_str()));
    if (multiUserFunc)
    {
        LispDefFile* def = multiUserFunc->iFileToOpen;
        if (def)
        {
            RESULT = (LispAtom::New(aEnvironment,def->iFileName->c_str()));
      return;
        }
    }
    RESULT = (LispAtom::New(aEnvironment,"\"\""));
}

/// Corresponds to the Yacas function \c PatternCreate .
/// This function constructs a new PatternClass, and puts it in a new
/// LispGenericObject. The result is set to this LispGenericObject.
void GenPatternCreate(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr pattern(ARGUMENT(1));
    LispPtr postpredicate(ARGUMENT(2));

    LispIterator iter(pattern);
    LispObject * pObj = iter.getObj();
    CHK_ARG_CORE(pObj,1);  // Check(pObj,KLispErrInvalidArg);
    LispPtr * pPtr = pObj->SubList();
    CHK_ARG_CORE(pPtr,1);  // Check(pPtr,KLispErrNotList);
    iter = *pPtr;
    CHK_ARG_CORE(iter.getObj(),1);
    ++iter;

    YacasPatternPredicateBase* matcher =
        NEW YacasPatternPredicateBase(aEnvironment, *iter,postpredicate);
    PatternClass *p = NEW PatternClass(matcher);
    RESULT = (LispGenericClass::New(p));
}

void GenPatternMatches(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  LispPtr pattern(ARGUMENT(1));
  GenericClass *gen = pattern->Generic();
  DYNCAST(PatternClass,"\"Pattern\"",patclass,gen)
  CHK_ARG_CORE(patclass,1);

  LispPtr list(ARGUMENT(2));

  LispIterator iter(list);
  LispObject * pObj = iter.getObj();
  CHK_ARG_CORE(pObj,2);
  LispPtr * pPtr = pObj->SubList();
  CHK_ARG_CORE(pPtr,2);
  iter = *pPtr;
  CHK_ARG_CORE(iter.getObj(),2);
  ++iter;

  CHK_ARG_CORE(iter.getObj(),2);
  LispBoolean matches = patclass->Matches(aEnvironment,*iter);
  InternalBoolean(aEnvironment,RESULT,matches);
}

void LispRuleBaseDefined(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr name(ARGUMENT(1));
    LispString * orig = name->String();
    CHK_ARG_CORE(orig, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispPtr sizearg(ARGUMENT(2));
    CHK_ARG_CORE(sizearg, 2);
    CHK_ARG_CORE(sizearg->String(), 2);

    LispInt arity = InternalAsciiToInt(sizearg->String());

    LispUserFunction* userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper.c_str()),arity);
    InternalBoolean(aEnvironment,RESULT,!!userFunc);
}

void LispDefLoadFunction(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr name(ARGUMENT(1));
    LispString * orig = name->String();
    CHK_ARG_CORE(orig, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper.c_str()));
    if (multiUserFunc)
    {
        if (multiUserFunc->iFileToOpen!=NULL)
        {
            LispDefFile* def = multiUserFunc->iFileToOpen;
            if (!def->iIsLoaded)
            {
#ifdef YACAS_DEBUG
                /*Show loading... */
                extern int verbose_debug;
                if (verbose_debug)
                    printf("Debug> Loading file %s for function %s\n",def->iFileName->c_str(),oper.c_str());
#endif
                multiUserFunc->iFileToOpen=NULL;
                InternalUse(aEnvironment,def->iFileName);
      }
        }
    }
    InternalTrue(aEnvironment,RESULT);
}

void LispRuleBaseArgList(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    LispPtr name(ARGUMENT(1));
    LispString * orig = name->String();
    CHK_ARG_CORE(orig, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispPtr sizearg(ARGUMENT(2));
    CHK_ARG_CORE(sizearg, 2);
    CHK_ARG_CORE(sizearg->String(), 2);

    LispInt arity = InternalAsciiToInt(sizearg->String());

    LispUserFunction* userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper.c_str()),arity);
    CHK_CORE(userFunc, KLispErrInvalidArg);

    LispPtr& list = userFunc->ArgList();
    LispPtr head(aEnvironment.iList->Copy());
    head->Nixed() = (list);
    RESULT = (LispSubList::New(head));
}

static void InternalNewRulePattern(LispEnvironment& aEnvironment, LispInt aStackTop, LispBoolean aMacroMode)
{
    LispInt arity;
    LispInt precedence;

    LispPtr ar;
    LispPtr pr;
    LispPtr predicate;
    LispPtr body;
    LispString * orig = NULL;
 
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1), 1);
    orig = ARGUMENT(1)->String();
    CHK_ARG_CORE(orig, 1);
    ar = (ARGUMENT(2));
    pr = (ARGUMENT(3));
    predicate = (ARGUMENT(4));
    body = (ARGUMENT(5));
 
    // The arity
    CHK_ARG_CORE(ar, 2);
    CHK_ARG_CORE(ar->String(), 2);
    arity = InternalAsciiToInt(ar->String());

    // The precedence
    CHK_ARG_CORE(pr, 3);
    CHK_ARG_CORE(pr->String(), 3);
    precedence = InternalAsciiToInt(pr->String());
 
    // Finally define the rule base
    aEnvironment.DefineRulePattern(SymbolName(aEnvironment,orig->c_str()),
                            arity,
                            precedence,
                            predicate,
                            body );

    // Return LispTrue
    InternalTrue(aEnvironment,RESULT);
}

void LispNewRulePattern(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalNewRulePattern(aEnvironment, aStackTop, LispFalse);
}

void LispMacroNewRulePattern(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalNewRulePattern(aEnvironment, aStackTop, LispTrue);
}
