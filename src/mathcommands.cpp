
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
//TODO remove #include "anumber.h"
#include "arrayclass.h"
#include "patternclass.h"
#include "substitute.h"
#include "errors.h"
#include "arggetter.h"

#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)


void LispLexCompare2(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispBoolean (*lexfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision),
#ifndef NO_USE_BIGFLOAT
                     LispBoolean (*numfunc)(BigNumber& n1, BigNumber& n2)
#else
                     LispBoolean (*numfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision)
#endif
                    );




void LispQuote(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    RESULT.Set(ARGUMENT(1).Get()->Copy(LispFalse));
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
static void InternalSetVar(LispEnvironment& aEnvironment, LispInt aStackTop, LispBoolean aMacroMode)
{
    LispStringPtr varstring=NULL;
    if (aMacroMode)
    {
        LispPtr result;
        InternalEval(aEnvironment, result, ARGUMENT(1));
        varstring = result.Get()->String();
    }
    else
    {
        varstring = ARGUMENT(1).Get()->String();
    }
    CHK_ARG_CORE(varstring != NULL,1);
    CHK_ARG_CORE(!IsNumber(varstring->String(),LispTrue),1);
    
    LispPtr result;
    InternalEval(aEnvironment, result, ARGUMENT(2));
    aEnvironment.SetVariable(varstring, result);
    InternalTrue(aEnvironment,RESULT);
}

/// Corresponds to the Yacas function \c Set.
/// This function simply calls InternalSetVar() .
void LispSetVar(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalSetVar(aEnvironment, aStackTop, LispFalse);
}

/// Corresponds to the Yacas function \c MacroSet.
/// This function simply calls InternalSetVar() .
void LispMacroSetVar(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    InternalSetVar(aEnvironment, aStackTop, LispTrue);
}

void LispClearVar(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  LispPtr* subList = ARGUMENT(1).Get()->SubList();
  if (subList)
  {
    LispIterator iter(*subList);
    iter.GoNext();
    LispInt nr=1;
    while (iter())
    {
      LispStringPtr str;
      str = iter()->String();
      CHK_ARG_CORE(str != NULL, nr);
      aEnvironment.UnsetVariable(str);
      iter.GoNext();
      nr++;
    }
  }
  InternalTrue(aEnvironment,RESULT);
}



/* StrCompare returns f1-f2: if f1 < f2 it returns -1, if f1=f2 it
 returns 0, and it returns 1 if f1>f2
 */
// the aPrecision argument is ignored here
static LispBoolean LexLessThan(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision)
{
    return (StrCompare(f1, f2)<0);
}

// the aPrecision argument is ignored here
static LispBoolean LexGreaterThan(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision)
{
    return (StrCompare(f1, f2)>0);
}

#ifndef NO_USE_BIGFLOAT
static LispBoolean BigLessThan(BigNumber& n1, BigNumber& n2)
{
  return n1.LessThan(n2) && !n1.Equals(n2);
}
static LispBoolean BigGreaterThan(BigNumber& n1, BigNumber& n2)
{
  return !(n1.LessThan(n2) || n1.Equals(n2));
}
#endif

void LispLessThan(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
    LispLexCompare2(aEnvironment, aStackTop, LexLessThan,BigLessThan);
#else
    LispLexCompare2(aEnvironment, aStackTop, LexLessThan,LessThan);
#endif
}

void LispGreaterThan(LispEnvironment& aEnvironment, LispInt aStackTop)
{
#ifndef NO_USE_BIGFLOAT
    LispLexCompare2(aEnvironment, aStackTop, LexGreaterThan, BigGreaterThan);
#else
    LispLexCompare2(aEnvironment, aStackTop, LexGreaterThan, GreaterThan);
#endif
}


void LispLexCompare2(LispEnvironment& aEnvironment, LispInt aStackTop,
                     LispBoolean (*lexfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision),
#ifndef NO_USE_BIGFLOAT
                     LispBoolean (*numfunc)(BigNumber& n1, BigNumber& n2)
#else
                     LispBoolean (*numfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision)
#endif
                    )
{
    //TESTARGS(3);
    LispPtr result1;
    LispPtr result2;
    result1.Set(ARGUMENT(1).Get());
    result2.Set(ARGUMENT(2).Get());
    LispBoolean cmp;
#ifndef NO_USE_BIGFLOAT
    RefPtr<BigNumber> n1; n1 = result1.Get()->Number(aEnvironment.Precision());
    RefPtr<BigNumber> n2; n2 = result2.Get()->Number(aEnvironment.Precision());
    if (n1.Ptr() != NULL && n2.Ptr() != NULL)
    {
      cmp =numfunc(*n1.Ptr(),*n2.Ptr());
    }
#else
    LispStringPtr str1;
    LispStringPtr str2;
    str1 = result1.Get()->String();
    str2 = result2.Get()->String();
    CHK_ARG_CORE(str1 != NULL ,1);
    CHK_ARG_CORE(str2 != NULL, 2);
    if (IsNumber(str1->String(),LispTrue) &&
        IsNumber(str2->String(),LispTrue))
    {
      cmp =numfunc(str1->String(),str2->String(),
                            aEnvironment.HashTable(),
                            aEnvironment.Precision());
    }
#endif
    else
    {
#ifndef NO_USE_BIGFLOAT
      LispStringPtr str1;
      LispStringPtr str2;
      str1 = result1.Get()->String();
      str2 = result2.Get()->String();
      CHK_ARG_CORE(str1 != NULL ,1);
      CHK_ARG_CORE(str2 != NULL, 2);
#endif
	  // the precision argument is ignored in "lex" functions
      cmp =lexfunc(str1->String(),str2->String(),
                            aEnvironment.HashTable(),
                            aEnvironment.Precision());
    }
    
    InternalBoolean(aEnvironment,RESULT, cmp);
}

// this function will eventually be moved to scripts
void LispPi(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(1);
    RESULT.Set(PiFloat(aEnvironment, aEnvironment.Precision()));
}




void LispFullForm(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    RESULT.Set(ARGUMENT(1).Get());
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
    LispStringPtr str;
    str = ARGUMENT(2).Get()->String();
    CHK_ARG_CORE(str != NULL,2);
    CHK_ARG_CORE(IsNumber(str->String(),LispFalse),2);
    LispInt index = InternalAsciiToInt(str->String());
    InternalNth(RESULT, ARGUMENT(1), index);
}


void LispTail(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr first;
    InternalTail(first, ARGUMENT(1));
    InternalTail(RESULT, first);
    LispPtr head;
    head.Set(aEnvironment.iList->Copy(LispFalse));
    head.Get()->Next().Set(RESULT.Get()->SubList()->Get());
    RESULT.Get()->SubList()->Set(head.Get());
}

void LispUnList(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  CHK_ARG_CORE(ARGUMENT(1).Get(), 1);
  CHK_ARG_CORE(ARGUMENT(1).Get()->SubList(), 1);
  LispObject* subList = ARGUMENT(1).Get()->SubList()->Get();
  CHK_ARG_CORE(subList, 1);
  CHK_ARG_CORE(subList->String() == aEnvironment.iList->String(),1);
  InternalTail(RESULT, ARGUMENT(1));
}

void LispListify(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  CHK_ARG_CORE(ARGUMENT(1).Get()->SubList(), 1);
  LispPtr head;
  head.Set(aEnvironment.iList->Copy(LispFalse));
  head.Get()->Next().Set(ARGUMENT(1).Get()->SubList()->Get());
  RESULT.Set(LispSubList::New(head.Get()));
}




void LispDestructiveReverse(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr reversed;
  reversed.Set(aEnvironment.iList->Copy(LispFalse));
  InternalReverseList(reversed.Get()->Next(), ARGUMENT(1).Get()->SubList()->Get()->Next());
  RESULT.Set(LispSubList::New(reversed.Get()));
}


void LispLength(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr* subList = ARGUMENT(1).Get()->SubList();
  if (subList != NULL)
  {
    LispChar s[20];
    LispInt num = InternalListLength(subList->Get()->Next());
    InternalIntToAscii(s,num);
    RESULT.Set(LispAtom::New(aEnvironment,s));
    return;
  }
  LispStringPtr string = ARGUMENT(1).Get()->String();
  if (InternalIsString(string))
  {
    LispChar s[20];
    LispInt num = string->NrItems()-3;
    InternalIntToAscii(s,num);
    RESULT.Set(LispAtom::New(aEnvironment,s));
    return;
  }
  GenericClass *gen = ARGUMENT(1).Get()->Generic();
  if (gen != NULL)
  if (StrEqual(gen->TypeName(),"\"Array\""))
  {
    LispInt size=((ArrayClass*)gen)->Size();
    LispChar s[20];
    InternalIntToAscii(s,size);
    RESULT.Set(LispAtom::New(aEnvironment,s));
    return;
  }
//  CHK_ISLIST_CORE(ARGUMENT(1),1);
}

void LispList(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr all;
  all.Set(aEnvironment.iList->Copy(LispFalse));
  LispIterator tail(all);
  tail.GoNext();
  LispIterator iter(*ARGUMENT(1).Get()->SubList());
  iter.GoNext();
  while (iter())
  {
    LispPtr evaluated;
    InternalEval(aEnvironment,evaluated,*iter.Ptr());
    tail.Ptr()->Set(evaluated.Get());
    tail.GoNext();
    iter.GoNext();
  }
  RESULT.Set(LispSubList::New(all.Get()));
}


void LispConcatenate(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr all;
  all.Set(aEnvironment.iList->Copy(LispFalse));
  LispIterator tail(all);
  tail.GoNext();
  LispInt arg = 1;

  LispIterator iter(*ARGUMENT(1).Get()->SubList());
  iter.GoNext();
  while (iter())
  {
    CHK_ISLIST_CORE(*iter.Ptr(),arg);
    InternalFlatCopy(*tail.Ptr(),iter.Ptr()->Get()->SubList()->Get()->Next());
    while (tail() != NULL)
      tail.GoNext();
    iter.GoNext();
    arg++;
  }
  RESULT.Set(LispSubList::New(all.Get()));
}


static void ConcatenateStrings(LispStringSmartPtr& aSmartPtr, LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LISPASSERT(aSmartPtr());
    aSmartPtr()->SetNrItems(0);
    aSmartPtr()->Append('\"');
    LispInt arg=1;

/*
{
LispString res;
PrintExpression(res, ARGUMENT(1),aEnvironment,100);
printf("%s\n",res.String());
}
*/ 
   
    LispIterator iter(*ARGUMENT(1).Get()->SubList());
    iter.GoNext();
    while (iter())
    {
        CHK_ISSTRING_CORE(*iter.Ptr(),arg);

        LispInt length = iter()->String()->NrItems()-2;
        LispCharPtr ptr=iter()->String()->String();
        LispInt curlen = aSmartPtr()->NrItems();
        aSmartPtr()->GrowTo(curlen+length-1);
        LispCharPtr put = &(*aSmartPtr())[curlen-1];
        PlatMemCopy(put+1,ptr+1,length-1);
        iter.GoNext();
        arg++;
    }
    
    aSmartPtr()->Append('\"');
    aSmartPtr()->Append('\0');
}
void LispConcatenateStrings(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispString *str = NEW LispString;
    LispStringSmartPtr smartptr;
    smartptr.Set(str);
    ConcatenateStrings(smartptr,aEnvironment, aStackTop);
    RESULT.Set(LispAtom::New(aEnvironment,str->String()));
}

static void InternalDelete(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aDestructive)
{
    //TESTARGS(3);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());
    CHK_ISLIST_CORE(evaluated,1);

    LispPtr copied;
    if (aDestructive)
    {
        copied.Set(evaluated.Get()->SubList()->Get());
    }
    else
    {
        InternalFlatCopy(copied,*evaluated.Get()->SubList());
    }

    LispPtr index;
    index.Set(ARGUMENT(2).Get());
    CHK_ARG_CORE(index.Get() != NULL, 2);
    CHK_ARG_CORE(index.Get()->String() != NULL, 2);
    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());
    CHK_ARG_CORE(ind>0,2);

    LispIterator iter(copied);
    while (ind>0)
    {
        iter.GoNext();
        ind--;
    }
    CHK_CORE(iter() != NULL, KLispErrListNotLongEnough);
    LispPtr next;
    next.Set(iter()->Next().Get());
    iter.Ptr()->Set(next.Get());
    RESULT.Set(LispSubList::New(copied.Get()));
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
    InternalFlatCopy(copied,*ARGUMENT(1).Get()->SubList());
    RESULT.Set(LispSubList::New(copied.Get()));
}


static void InternalInsert(LispEnvironment& aEnvironment, LispInt aStackTop, LispInt aDestructive)
{
    //TESTARGS(4);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());
    CHK_ISLIST_CORE(evaluated,1);

    LispPtr copied;
    if (aDestructive)
    {
        copied.Set(evaluated.Get()->SubList()->Get());
    }
    else
    {
        InternalFlatCopy(copied,*evaluated.Get()->SubList());
    }
    
    LispPtr index;
    index.Set(ARGUMENT(2).Get());
    CHK_ARG_CORE(index.Get() != NULL, 2);
    CHK_ARG_CORE(index.Get()->String() != NULL, 2);
    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());
    CHK_ARG_CORE(ind>0,2);

    LispIterator iter(copied);
    while (ind>0)
    {
        iter.GoNext();
        ind--;
    }

    LispPtr toInsert;
    toInsert.Set(ARGUMENT(3).Get());
    toInsert.Get()->Next().Set(iter());
    iter.Ptr()->Set(toInsert.Get());
    RESULT.Set(LispSubList::New(copied.Get()));
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
    //TESTARGS(4);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());
//    CHK_ISLIST_CORE(evaluated,1);
    // Ok, so lets not check if it is a list, but it needs to be at least a 'function'
    CHK_ARG_CORE(evaluated.Get()->SubList() != NULL, 1);

    LispPtr index;
    index.Set(ARGUMENT(2).Get());
    CHK_ARG_CORE(index.Get() != NULL, 2);
    CHK_ARG_CORE(index.Get()->String() != NULL, 2);
    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());

    LispPtr copied;
    if (aDestructive)
    {
        copied.Set(evaluated.Get()->SubList()->Get());
    }
    else
    {
        InternalFlatCopy(copied,*evaluated.Get()->SubList());
    }
    CHK_ARG_CORE(ind>0,2);

    LispIterator iter(copied);
    while (ind>0)
    {
        iter.GoNext();
        ind--;
    }

    LispPtr toInsert;
    toInsert.Set(ARGUMENT(3).Get());
    CHK_ARG_CORE(iter.Ptr() != NULL, 2);
    CHK_ARG_CORE(iter.Ptr()->Get() != NULL, 2);
    toInsert.Get()->Next().Set(iter.Ptr()->Get()->Next().Get());
    iter.Ptr()->Set(toInsert.Get());
    RESULT.Set(LispSubList::New(copied.Get()));
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
    //TESTARGS(2);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());
    if (IsTrue(aEnvironment, evaluated) || IsFalse(aEnvironment, evaluated))
    {
        InternalNot(RESULT, aEnvironment, evaluated);
    }
    else
    {
        LispPtr ptr;
        ptr.Set(ARGUMENT(0).Get()->Copy(LispFalse));
        ptr.Get()->Next().Set(evaluated.Get());
        RESULT.Set(LispSubList::New(ptr.Get()));
    }
}

void LispLazyAnd(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispPtr nogos;
    LispInt nrnogos=0;
    LispPtr evaluated;

    LispIterator iter(*ARGUMENT(1).Get()->SubList());
    iter.GoNext();
    while (iter())
    {
        InternalEval(aEnvironment, evaluated, *iter.Ptr());
        if (IsFalse(aEnvironment, evaluated))
        {
            InternalFalse(aEnvironment,RESULT);
            return;
        }
        else if (!IsTrue(aEnvironment, evaluated))
        {
            LispPtr ptr;
            nrnogos++;
            ptr.Set(evaluated.Get()->Copy(LispFalse));
            ptr.Get()->Next().Set(nogos.Get());
            nogos.Set(ptr.Get());
        }
        
        iter.GoNext();
    }

    if (nogos.Get() != NULL)
    {
        if (nrnogos == 1)
        {
            RESULT.Set(nogos.Get());
        }
        else
        {
            LispPtr ptr;

            InternalReverseList(ptr, nogos);
            nogos.Set(ptr.Get());

            ptr.Set(ARGUMENT(0).Get()->Copy(LispFalse));
            ptr.Get()->Next().Set(nogos.Get());
            nogos.Set(ptr.Get());
            RESULT.Set(LispSubList::New(nogos.Get()));

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
    LispInt nrnogos=0;

    LispPtr evaluated;

    LispIterator iter(*ARGUMENT(1).Get()->SubList());
    iter.GoNext();
    while (iter())
    {
        InternalEval(aEnvironment, evaluated, *iter.Ptr());
        if (IsTrue(aEnvironment, evaluated))
        {
            InternalTrue(aEnvironment,RESULT);
            return;
        }
        else if (!IsFalse(aEnvironment, evaluated))
        {


            LispPtr ptr;
            nrnogos++;

            ptr.Set(evaluated.Get()->Copy(LispFalse));
            ptr.Get()->Next().Set(nogos.Get());
            nogos.Set(ptr.Get());
        }
        iter.GoNext();
    }

    if (nogos.Get() != NULL)
    {
        if (nrnogos == 1)
        {
            RESULT.Set(nogos.Get());
        }
        else
        {
            LispPtr ptr;

            InternalReverseList(ptr, nogos);
            nogos.Set(ptr.Get());

            ptr.Set(ARGUMENT(0).Get()->Copy(LispFalse));
            ptr.Get()->Next().Set(nogos.Get());
            nogos.Set(ptr.Get());
            RESULT.Set(LispSubList::New(nogos.Get()));
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
    //TESTARGS(3);
    LispPtr evaluated1;
    evaluated1.Set(ARGUMENT(1).Get());
    LispPtr evaluated2;
    evaluated2.Set(ARGUMENT(2).Get());

    InternalBoolean(aEnvironment,RESULT,
                    InternalEquals(aEnvironment, evaluated1, evaluated2));
}


void LispWrite(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr* subList = ARGUMENT(1).Get()->SubList();
  if (subList)
  {
    LispIterator iter(*subList);
    iter.GoNext();
    while (iter())
    {
      aEnvironment.CurrentPrinter().Print(*iter.Ptr(),*aEnvironment.CurrentOutput(),aEnvironment);
      iter.GoNext();
    }
  }
  InternalTrue(aEnvironment,RESULT);
}

void LispWriteString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  CHK_ARG_CORE(ARGUMENT(1).Get()!= NULL,1);
  LispStringPtr str = ARGUMENT(1).Get()->String();
  CHK_ARG_CORE(str != NULL,1);
  CHK_ARG_CORE((*str)[0] == '\"',1);
  CHK_ARG_CORE((*str)[str->NrItems()-2] == '\"',1);

  LispInt i=1;
  LispInt nr=str->NrItems()-2;
  //((*str)[i] != '\"')
  for (i=1;i<nr;i++)
  {
    aEnvironment.CurrentOutput()->PutChar((*str)[i]);
  }
	// pass last printed character to the current printer
	aEnvironment.CurrentPrinter().RememberLastChar((*str)[nr-1]);	// hacky hacky
  InternalTrue(aEnvironment,RESULT);
}

void LispProgBody(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  // Allow accessing previous locals.
  LispLocalFrame frame(aEnvironment,LispFalse);

  InternalTrue(aEnvironment,RESULT);
  
  // Evaluate args one by one.

  LispIterator iter(*ARGUMENT(1).Get()->SubList());
  iter.GoNext();
  while (iter())
  {
    InternalEval(aEnvironment, RESULT, *iter.Ptr());
    iter.GoNext();
  }
}


void LispNewLocal(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  LispPtr* subList = ARGUMENT(1).Get()->SubList();
  if (subList)
  {
    LispIterator iter(*subList);
    iter.GoNext();

    LispInt nr = 1;
    while (iter())
    {
      LispStringPtr variable;
      variable = iter()->String();
      CHK_ARG_CORE(variable != NULL,nr);
// printf("Variable %s\n",variable->String());
      aEnvironment.NewLocal(variable,NULL);
      iter.GoNext();
      nr++;
    }
  }
  InternalTrue(aEnvironment,RESULT);
}

void LispWhile(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);

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
    //TESTARGS(3);

    // Get operator
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    LispStringPtr orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    
    LispPtr precedence;
    InternalEval(aEnvironment, precedence, ARGUMENT(2));
    CHK_ARG_CORE(precedence.Get()->String() != NULL, 2);
    LispInt prec = InternalAsciiToInt(precedence.Get()->String()->String());
    CHK_ARG_CORE(prec <= KMaxPrecedence, 2);
    aOps.SetOperator(prec,SymbolName(aEnvironment,orig->String()));
    InternalTrue(aEnvironment,RESULT);
}

void LispInFix(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    MultiFix(aEnvironment, aStackTop, aEnvironment.InFix());
}


static void SingleFix(LispInt aPrecedence, LispEnvironment& aEnvironment, LispInt aStackTop, LispOperators& aOps)
{
    //TESTARGS(2);

    // Get operator
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    LispStringPtr orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    aOps.SetOperator(aPrecedence,SymbolName(aEnvironment,orig->String()));
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
    //TESTARGS(2);

    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    // Get operator
    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpUnStringify(orig->String())->String()));
}


void LispStringify(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);

    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    // Get operator
    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(orig->String())->String()));
}




void LispLoad(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    // Get file name
    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    InternalLoad(aEnvironment,orig);
    InternalTrue(aEnvironment,RESULT);
}


/// Implements the Yacas functions \c RuleBase and \c MacroRuleBase .
/// The real work is done by LispEnvironment::DeclareRuleBase().
static void InternalRuleBase(LispEnvironment& aEnvironment, LispInt aStackTop, 
                             LispInt aListed)
{
    //TESTARGS(3);
    
    // Get operator
    LispPtr args;
    LispStringPtr orig=NULL;
    
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    args.Set(ARGUMENT(2).Get());
    
    // The arguments
    CHK_ISLIST_CORE(args,2);

    // Finally define the rule base
    aEnvironment.DeclareRuleBase(SymbolName(aEnvironment,orig->String()),
                                 args.Get()->SubList()->Get()->Next(),aListed);
    
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
    //TESTARGS(3);
    
    // Get operator
    LispPtr args;
    LispPtr body;
    LispStringPtr orig=NULL;
    
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    // The arguments
    args.Set(ARGUMENT(2).Get());
    CHK_ISLIST_CORE(args,2);

    // Finally define the rule base
    aEnvironment.DeclareMacroRuleBase(SymbolName(aEnvironment,orig->String()),
                                 args.Get()->SubList()->Get()->Next(),aListed);
    
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
    //TESTARGS(3);
    
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    LispStringPtr orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    // The arguments
    LispStringPtr tohold = ARGUMENT(2).Get()->String();
    CHK_ARG_CORE(tohold != NULL, 2);
    aEnvironment.HoldArgument(SymbolName(aEnvironment,orig->String()), tohold);
    // Return LispTrue
    InternalTrue(aEnvironment,RESULT);
}

static void InternalNewRule(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(6);

    LispInt arity;
    LispInt precedence;

    LispPtr ar;
    LispPtr pr;
    LispPtr predicate;
    LispPtr body;
    LispStringPtr orig=NULL;
    
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    ar.Set(ARGUMENT(2).Get());
    pr.Set(ARGUMENT(3).Get());
    predicate.Set(ARGUMENT(4).Get());
    body.Set(ARGUMENT(5).Get());
    
    // The arity
    CHK_ARG_CORE(ar.Get() != NULL, 2);
    CHK_ARG_CORE(ar.Get()->String() != NULL, 2);
    arity = InternalAsciiToInt(ar.Get()->String()->String());

    // The precedence
    CHK_ARG_CORE(pr.Get() != NULL, 3);
    CHK_ARG_CORE(pr.Get()->String() != NULL, 3);
    precedence = InternalAsciiToInt(pr.Get()->String()->String());
    
    // Finally define the rule base
    aEnvironment.DefineRule(SymbolName(aEnvironment,orig->String()),
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
    //TESTARGS(3);
    
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    LispStringPtr orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    // The arity
    CHK_ARG_CORE(ARGUMENT(2).Get() != NULL, 2);
    CHK_ARG_CORE(ARGUMENT(2).Get()->String() != NULL, 2);
    LispInt arity = InternalAsciiToInt(ARGUMENT(2).Get()->String()->String());

    aEnvironment.UnFenceRule(SymbolName(aEnvironment,orig->String()),
                            arity);
    
    // Return LispTrue
    InternalTrue(aEnvironment,RESULT);
}


void LispMathLibName(LispEnvironment& aEnvironment,LispInt aStackTop)
{
	//TESTARGS(1);
        // can't use NumericLibraryName() inside LookUpStringify() b/c of
        // nonconstant pointer! why is it not a const char* and do I have to
        // write this?
        // const_cast removes const-ness... ;-)
        char* library_name = const_cast<char*>(NumericLibraryName());
        RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(
		library_name
	)->String()));
}

void LispIsFunction(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr result;
    result.Set(ARGUMENT(1).Get());
    InternalBoolean(aEnvironment,RESULT,
                    result.Get()->SubList()!=NULL);
}
void LispIsAtom(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr result;
    result.Set(ARGUMENT(1).Get());
    InternalBoolean(aEnvironment,RESULT,
                    result.Get()->String()!=NULL);
}
void LispIsNumber(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr result;
    result.Set(ARGUMENT(1).Get());

#ifndef NO_USE_BIGFLOAT
    if (result.Get()->Number(aEnvironment.Precision()) == NULL)
    {
        InternalFalse(aEnvironment,RESULT);
    }
    else
    {
        InternalTrue(aEnvironment,RESULT);
    }
#else
    if (result.Get()->String() == NULL)
    {
        InternalFalse(aEnvironment,RESULT);
    }
    else
    {
        InternalBoolean(aEnvironment,RESULT,
                        IsNumber(result.Get()->String()->String(),LispTrue));
    }
#endif
}

void LispIsInteger(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr result;
    result.Set(ARGUMENT(1).Get());

#ifndef NO_USE_BIGFLOAT
/*
//    if (result.Get() == 0)
    {
      LispString str;
      PrintExpression(str, result, aEnvironment, 80);
      printf("%s\n",str.String());
    }
*/
    RefPtr<BigNumber> num ; num = result.Get()->Number(aEnvironment.Precision());
    if (num.Ptr() == NULL)
    {
        InternalFalse(aEnvironment,RESULT);
    }
    else if (!num->IsInt())
    {
        InternalFalse(aEnvironment,RESULT);
    }
    else
    {
        InternalTrue(aEnvironment,RESULT);
    }
#else
    if (result.Get()->String() == NULL)
    {
        InternalFalse(aEnvironment,RESULT);
    }
    else
    {
        InternalBoolean(aEnvironment,RESULT,
                        IsNumber(result.Get()->String()->String(),LispFalse));
    }
#endif
}


void LispIsList(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr result;
    result.Set(ARGUMENT(1).Get());
    InternalBoolean(aEnvironment,RESULT,InternalIsList(result));
}


void LispIsString(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr result;
    result.Set(ARGUMENT(1).Get());
    InternalBoolean(aEnvironment,RESULT,
                    InternalIsString(result.Get()->String()));
}

void LispIsBound(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispStringPtr str = ARGUMENT(1).Get()->String();
    if (str)
    {
        LispPtr val;
        aEnvironment.GetVariable(str,val);
        if (val.Get())
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
    //TESTARGS(3);

    // Get operator
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    LispStringPtr oper = SymbolName(aEnvironment,orig->String());
    
    LispPtr arity;
    arity.Set(ARGUMENT(2).Get());
    CHK_ARG_CORE(arity.Get()->String() != NULL, 2);
    LispInt ar = InternalAsciiToInt(arity.Get()->String()->String());
    aEnvironment.Retract(oper, ar);
    InternalTrue(aEnvironment,RESULT);
}


void LispPrecision(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);

    LispPtr index;
    index.Set(ARGUMENT(1).Get());
    CHK_ARG_CORE(index.Get() != NULL, 1);
    CHK_ARG_CORE(index.Get()->String() != NULL, 1);

    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());
    CHK_ARG_CORE(ind>0,1);
    aEnvironment.SetPrecision(ind);
    InternalTrue(aEnvironment,RESULT);
}



void LispDefaultDirectory(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Get file name
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    LispStringPtr orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);
    aEnvironment.iInputDirectories.Append(NEW LispString(oper.String()));
    InternalTrue(aEnvironment,RESULT);
}


void LispDllDirectory(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    // Get file name
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    LispStringPtr orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);
    aEnvironment.iDllDirectories.Append(NEW LispString(oper.String()));
    InternalTrue(aEnvironment,RESULT);
}


void LispFromFile(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);
        LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, ARGUMENT(1));

    // Get file name
    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    LispStringPtr contents = aEnvironment.FindCachedFile(orig->String());
    LispStringPtr hashedname = aEnvironment.HashTable().LookUpUnStringify(orig->String());

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname->String());

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
        //TODO make the file api platform independent!!!!
        // Open file
        LispLocalFile localFP(aEnvironment, hashedname->String(),LispTrue,
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
    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
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
    LispStringPtr result;
    result = tok.NextToken(*aEnvironment.CurrentInput(),
                           aEnvironment.HashTable());

    if (result->String()[0] == '\0')
    {
        RESULT.Set(aEnvironment.iEndOfFile->Copy(LispFalse));
        return;
    }
    RESULT.Set(LispAtom::New(aEnvironment,result->String()));
}


void LispToFile(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, ARGUMENT(1));

    // Get file name
    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    //TODO make the file api platform independent!!!!
    // Open file for writing
    LispLocalFile localFP(aEnvironment, oper.String(),LispFalse,
                          aEnvironment.iInputDirectories);
    CHK_CORE(localFP.iOpened != 0, KLispErrFileNotFound);
    StdFileOutput newOutput(localFP);
    LispLocalOutput localOutput(aEnvironment, &newOutput);

    // Evaluate the body
    InternalEval(aEnvironment, RESULT, ARGUMENT(2));

    //Return the result
}



void LispCheck(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(3);
    LispPtr pred;
    InternalEval(aEnvironment, pred, ARGUMENT(1));
    if (!IsTrue(aEnvironment,pred))
    {
        LispPtr evaluated;
        InternalEval(aEnvironment, evaluated, ARGUMENT(2));
        CHK_ISSTRING_CORE(evaluated,2);
        aEnvironment.SetUserError(evaluated.Get()->String()->String());
        CHK_CORE(0,KLispErrUser);
    }
    RESULT.Set(pred.Get());
}


void LispTrapError(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  //TESTARGS(3);
  LispTrap(
  {
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
  },aEnvironment.iErrorOutput,aEnvironment);

  if (aEnvironment.iError[0])
  {
    InternalEval(aEnvironment, RESULT, ARGUMENT(2));
    aEnvironment.iError.SetNrItems(1);
    aEnvironment.iError[0]='\0';
  }
}

void LispGetCoreError(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(aEnvironment.iError.String())->String()));
}



void LispSystemCall(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr result;
    result.Set(ARGUMENT(1).Get());
    CHK_ISSTRING_CORE(result,1);

    LispString command;
    InternalUnstringify(command, result.Get()->String());

// we would like to pass the exit code back to Yacas. Right now, let's pass True/False according to whether the exit code is 0 or not.
#ifdef SystemCall
	if(SystemCall(command.String()) == 0)
	{	
	    InternalTrue(aEnvironment,RESULT);
	}
	else
	{
	    InternalFalse(aEnvironment,RESULT);
	}
#else
    InternalFalse(aEnvironment,RESULT);
#endif
}





// this function will eventually be removed
void LispFastPi(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(1);
    RESULT.Set(PlatPi(aEnvironment,aEnvironment.Precision()));
}













void LispMaxEvalDepth(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);

    LispPtr index;
    index.Set(ARGUMENT(1).Get());
    CHK_ARG_CORE(index.Get() != NULL, 1);
    CHK_ARG_CORE(index.Get()->String() != NULL, 1);

    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());
    aEnvironment.iMaxEvalDepth = ind;
    InternalTrue(aEnvironment,RESULT);
}


void LispDefLoad(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);
    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    // Get file name
    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    LoadDefFile(aEnvironment, orig);
    InternalTrue(aEnvironment,RESULT);
}

void LispUse(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);
//This one seems safe...    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    // Get file name
    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    InternalUse(aEnvironment,orig);
    InternalTrue(aEnvironment,RESULT);
}

void LispRightAssociative(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(2);
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    LispStringPtr orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    aEnvironment.InFix().SetRightAssociative(SymbolName(aEnvironment,orig->String()));
    InternalTrue(aEnvironment,RESULT);
}


void LispLeftPrecedence(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    LispStringPtr orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    LispPtr index;
    InternalEval(aEnvironment, index, ARGUMENT(2));
    CHK_ARG_CORE(index.Get() != NULL, 2);
    CHK_ARG_CORE(index.Get()->String() != NULL, 2);
    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());

    aEnvironment.InFix().SetLeftPrecedence(SymbolName(aEnvironment,orig->String()),ind);
    InternalTrue(aEnvironment,RESULT);
}


void LispRightPrecedence(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(3);
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    LispStringPtr orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    LispPtr index;
    InternalEval(aEnvironment, index, ARGUMENT(2));
    CHK_ARG_CORE(index.Get() != NULL, 2);
    CHK_ARG_CORE(index.Get()->String() != NULL, 2);
    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());

    aEnvironment.InFix().SetRightPrecedence(SymbolName(aEnvironment,orig->String()),ind);
    InternalTrue(aEnvironment,RESULT);
}
















static LispInFixOperator* OperatorInfo(LispEnvironment& aEnvironment,LispInt aStackTop, LispOperators& aOperators)
{
    //TESTARGS(2);
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);

    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);

    //
    LispInFixOperator* op = aOperators.LookUp(
                                              SymbolName(aEnvironment,orig->String()));
    return op;
}


void LispIsInFix(LispEnvironment& aEnvironment, LispInt aStackTop)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.InFix());
    if (op != NULL)
        InternalTrue( aEnvironment, RESULT);
    else
        InternalFalse(aEnvironment, RESULT);
}

void LispIsBodied(LispEnvironment& aEnvironment, LispInt aStackTop)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.Bodied());
    if (op != NULL)
        InternalTrue( aEnvironment, RESULT);
    else
        InternalFalse(aEnvironment, RESULT);
}

void LispGetPrecedence(LispEnvironment& aEnvironment, LispInt aStackTop)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.InFix());
    if (op == NULL) {	// also need to check for a postfix or prefix operator
	    op = OperatorInfo(aEnvironment,
                          aStackTop,
                          aEnvironment.PreFix());
        if (op == NULL) {
			op = OperatorInfo(aEnvironment,
                              aStackTop,
                              aEnvironment.PostFix());
	        if (op == NULL) {	// or maybe it's a bodied function
				op = OperatorInfo(aEnvironment,
                              aStackTop,
                              aEnvironment.Bodied());
    	 		CHK_CORE(op!=NULL, KLispErrIsNotInFix);
			}
		}
	}
    LispChar buf[30];
    InternalIntToAscii(buf, op->iPrecedence);
    RESULT.Set(LispAtom::New(aEnvironment,buf));
}


void LispGetLeftPrecedence(LispEnvironment& aEnvironment, LispInt aStackTop)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.InFix());
    if (op == NULL) {	// infix and postfix operators have left precedence
	    op = OperatorInfo(aEnvironment,
                          aStackTop,
                          aEnvironment.PostFix());
   	 	CHK_CORE(op!=NULL, KLispErrIsNotInFix);
	}

    LispChar buf[30];
    InternalIntToAscii(buf, op->iLeftPrecedence);
    RESULT.Set(LispAtom::New(aEnvironment,buf));
}
void LispGetRightPrecedence(LispEnvironment& aEnvironment, LispInt aStackTop)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.InFix());
    if (op == NULL) {   // bodied, infix and prefix operators have right precedence
        op = OperatorInfo(aEnvironment,
                          aStackTop,
                          aEnvironment.PreFix());
        if (op == NULL) {   // or maybe it's a bodied function
            op = OperatorInfo(aEnvironment,
                          aStackTop,
                          aEnvironment.Bodied());
            CHK_CORE(op!=NULL, KLispErrIsNotInFix);
        }
    }

    LispChar buf[30];
    InternalIntToAscii(buf, op->iRightPrecedence);
    RESULT.Set(LispAtom::New(aEnvironment,buf));
}



void LispIsPreFix(LispEnvironment& aEnvironment, LispInt aStackTop)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.PreFix());
    if (op != NULL)
        InternalTrue( aEnvironment, RESULT);
    else
        InternalFalse(aEnvironment, RESULT);
}

void LispIsPostFix(LispEnvironment& aEnvironment, LispInt aStackTop)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.PostFix());
    if (op != NULL)
        InternalTrue( aEnvironment, RESULT);
    else
        InternalFalse(aEnvironment, RESULT);
}

void LispGetPrecision(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    //TESTARGS(1);
    LispChar buf[30];
	// decimal precision
    InternalIntToAscii(buf, aEnvironment.Precision());
    RESULT.Set(LispAtom::New(aEnvironment,buf));
}



void LispToString(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispString oper;
    StringOutput newOutput(oper);

    LispLocalOutput localOutput(aEnvironment, &newOutput);

    // Evaluate the body
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));

    //Return the result
    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(oper.String())->String()));
}


void LispToStdout(LispEnvironment& aEnvironment, LispInt aStackTop)
{
    LispLocalOutput localOutput(aEnvironment, aEnvironment.iInitialOutput);
    // Evaluate the body
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
}


void LispSecure(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispSecureFrame security(aEnvironment);
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
}


void LispFindFile(LispEnvironment& aEnvironment,LispInt aStackTop)
{

    //TESTARGS(2);

    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);
    
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    // Get file name
    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispChar filename[1024];//TODO FIXME
    InternalFindFile(oper.String(), aEnvironment.iInputDirectories,
                     filename);
    LispString res(filename,1);
    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(res.String())->String()));
}


void LispIsGeneric(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    if (evaluated.Get()->Generic() != NULL)
        InternalTrue( aEnvironment, RESULT);
    else
        InternalFalse(aEnvironment, RESULT);
}

void LispGenericTypeName(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    CHK_ARG_CORE(evaluated.Get()->Generic() != NULL,1);

    LispCharPtr name = evaluated.Get()->Generic()->TypeName();
    RESULT.Set(LispAtom::New(aEnvironment,name));
}

void GenArrayCreate(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(3);

    LispPtr sizearg;
    sizearg.Set(ARGUMENT(1).Get());

    CHK_ARG_CORE(sizearg.Get() != NULL, 1);
    CHK_ARG_CORE(sizearg.Get()->String() != NULL, 1);

    LispInt size = InternalAsciiToInt(sizearg.Get()->String()->String());

    LispPtr initarg;
    initarg.Set(ARGUMENT(2).Get());
     
    ArrayClass *array = NEW ArrayClass(size,initarg.Get());
    RESULT.Set(LispGenericClass::New(array));
}

void GenArraySize(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    GenericClass *gen = evaluated.Get()->Generic();
    CHK_ARG_CORE(gen != NULL,1);
    CHK_ARG_CORE(StrEqual(gen->TypeName(),"\"Array\""),1);
    LispInt size=((ArrayClass*)gen)->Size();
    LispChar s[20];
    InternalIntToAscii(s,size);
    RESULT.Set(LispAtom::New(aEnvironment,s));
}

void GenArrayGet(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(3);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    GenericClass *gen = evaluated.Get()->Generic();
    CHK_ARG_CORE(gen != NULL,1);
    CHK_ARG_CORE(StrEqual(gen->TypeName(),"\"Array\""),1);

    LispPtr sizearg;
    sizearg.Set(ARGUMENT(2).Get());

    CHK_ARG_CORE(sizearg.Get() != NULL, 2);
    CHK_ARG_CORE(sizearg.Get()->String() != NULL, 2);

    LispInt size = InternalAsciiToInt(sizearg.Get()->String()->String());


    CHK_ARG_CORE(size>0 && size<=((ArrayClass*)gen)->Size(),2);
    LispObject* object = ((ArrayClass*)gen)->GetElement(size);

    RESULT.Set(object->Copy(LispFalse));
}


void GenArraySet(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(4);

    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    GenericClass *gen = evaluated.Get()->Generic();
    CHK_ARG_CORE(gen != NULL,1);
    CHK_ARG_CORE(StrEqual(gen->TypeName(),"\"Array\""),1);

    LispPtr sizearg;
    sizearg.Set(ARGUMENT(2).Get());

    CHK_ARG_CORE(sizearg.Get() != NULL, 2);
    CHK_ARG_CORE(sizearg.Get()->String() != NULL, 2);

    LispInt size = InternalAsciiToInt(sizearg.Get()->String()->String());
    CHK_ARG_CORE(size>0 && size<=((ArrayClass*)gen)->Size(),2);

    LispPtr obj;
    obj.Set(ARGUMENT(3).Get());
    ((ArrayClass*)gen)->SetElement(size,obj.Get());
    InternalTrue( aEnvironment, RESULT);
}

void LispCustomEval(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  //TESTARGS(5);
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
  //TESTARGS(1);
  if (aEnvironment.iDebugger == NULL)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  RESULT.Set(aEnvironment.iDebugger->iTopExpr.Get()); 
}
void LispCustomEvalResult(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  //TESTARGS(1);
  if (aEnvironment.iDebugger == NULL)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  RESULT.Set(aEnvironment.iDebugger->iTopResult.Get()); 
}



void LispCustomEvalLocals(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  //TESTARGS(1);
  aEnvironment.CurrentLocals(RESULT);
}

void LispCustomEvalStop(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  //TESTARGS(1);
  if (aEnvironment.iDebugger == NULL)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  aEnvironment.iDebugger->iStopped = LispTrue;

  InternalTrue(aEnvironment,RESULT);
}

void LispTraceStack(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
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
    //TESTARGS(3);
    LispPtr *ptr = ARGUMENT(0).Get()->Next().Get()->SubList();
    LispUserFunction* userfunc=NULL;
    if (ptr != NULL)
        userfunc = GetUserFunction(aEnvironment,ptr);
    LispLocalTrace trace(userfunc);
    InternalEval(aEnvironment, RESULT, ARGUMENT(2));
}

void LispType(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());
    LispPtr* subList = evaluated.Get()->SubList();
    LispObject* head = NULL;
    if (!subList)
    {
        goto EMPTY;
    }
    head = subList->Get();
    if (!head->String())
        goto EMPTY;
    RESULT.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(head->String()->String())->String()));
    return;
    
EMPTY:
    RESULT.Set(LispAtom::New(aEnvironment,"\"\""));
    return;
}



void LispStringMid(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(4);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(3).Get());
    CHK_ISSTRING_CORE(evaluated,3);
    LispStringPtr orig = evaluated.Get()->String();
    
    LispPtr index;
    index.Set(ARGUMENT(1).Get());
    CHK_ARG_CORE(index.Get() != NULL, 1);
    CHK_ARG_CORE(index.Get()->String() != NULL, 1);
    LispInt from = InternalAsciiToInt(index.Get()->String()->String());
    CHK_ARG_CORE(from>0,1);
    
    index.Set(ARGUMENT(2).Get());
    CHK_ARG_CORE(index.Get() != NULL, 2);
    CHK_ARG_CORE(index.Get()->String() != NULL, 2);
    LispInt count = InternalAsciiToInt(index.Get()->String()->String());

    
    LispString str;
    str.SetNrItems(0);
    str.Append('\"');
    LispInt i;
    CHK_CORE(from+count<orig->NrItems()-1, KLispErrInvalidArg);
    for (i=from;i<from+count;i++)
        str.Append((*orig)[i]);
    str.Append('\"');
    str.Append('\0');
    RESULT.Set(LispAtom::New(aEnvironment,str.String()));
}


void LispSetStringMid(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(4);
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(3).Get());
    CHK_ISSTRING_CORE(evaluated,3);
    LispStringPtr orig = evaluated.Get()->String();
    LispPtr index;
    index.Set(ARGUMENT(1).Get());
    CHK_ARG_CORE(index.Get() != NULL, 1);
    CHK_ARG_CORE(index.Get()->String() != NULL, 1);
    LispInt from = InternalAsciiToInt(index.Get()->String()->String());

    CHK_ARG_CORE(from>0,1);
    
    LispPtr ev2;
    ev2.Set(ARGUMENT(2).Get());
    CHK_ISSTRING_CORE(ev2,2);
    LispStringPtr replace = ev2.Get()->String();

    LispString str(orig->String());
    LispInt i;
    LispInt count = replace->NrItems();
    CHK_CORE(from+count-3<orig->NrItems()-1, KLispErrInvalidArg);

    for (i=0;i<count-3;i++)
        str[i+from] = (*replace)[i+1];
    RESULT.Set(LispAtom::New(aEnvironment,str.String()));
}



void LispFindFunction(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);
    
    LispPtr evaluated;
    evaluated.Set(ARGUMENT(1).Get());

    // Get file name
    CHK_ARG_CORE(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper.String()));
    if (multiUserFunc != NULL)
    {
        LispDefFile* def = multiUserFunc->iFileToOpen;
        if (def != NULL)
        {
            RESULT.Set(LispAtom::New(aEnvironment,def->iFileName()->String()));
            return;
        }
    }
    RESULT.Set(LispAtom::New(aEnvironment,"\"\""));
}

/// Corresponds to the Yacas function \c PatternCreate .
/// This function constructs a new PatternClass, and puts it in a new
/// LispGenericObject. The result is set to this LispGenericObject.
void GenPatternCreate(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(3);
    LispPtr pattern;
    pattern.Set(ARGUMENT(1).Get());
    LispPtr postpredicate;
    postpredicate.Set(ARGUMENT(2).Get());

    LispIterator iter(pattern);
    CHK_ARG_CORE(iter() != NULL,1);
    CHK_ARG_CORE(iter()->SubList() != NULL,1);
    iter.GoSub();
    CHK_ARG_CORE(iter() != NULL,1);
    iter.GoNext();

    LispPtr *ptr = iter.Ptr();


    YacasPatternPredicateBase* matcher =
        NEW YacasPatternPredicateBase(aEnvironment, *ptr,postpredicate);
    PatternClass *p = NEW PatternClass(matcher);
    RESULT.Set(LispGenericClass::New(p));
}
void GenPatternMatches(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(3);
    LispPtr pattern;
    pattern.Set(ARGUMENT(1).Get());
    GenericClass *gen = pattern.Get()->Generic();
    CHK_ARG_CORE(gen != NULL,1);
    CHK_ARG_CORE(StrEqual(gen->TypeName(),"\"Pattern\""),1);

    LispPtr list;
    list.Set(ARGUMENT(2).Get());

    PatternClass *patclass = (PatternClass*)gen;

    LispIterator iter(list);
    CHK_ARG_CORE(iter() != NULL,2);
    CHK_ARG_CORE(iter()->SubList() != NULL,2);
    iter.GoSub();
    CHK_ARG_CORE(iter() != NULL,2);
    iter.GoNext();

    LispPtr *ptr = iter.Ptr();
    CHK_ARG_CORE(ptr != NULL,2);
    LispBoolean matches = patclass->Matches(aEnvironment,*ptr);
    InternalBoolean(aEnvironment,RESULT,matches);
}

void LispRuleBaseDefined(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(3);
    LispPtr name;
    name.Set(ARGUMENT(1).Get());
    LispStringPtr orig = name.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispPtr sizearg;
    sizearg.Set(ARGUMENT(2).Get());
    CHK_ARG_CORE(sizearg.Get() != NULL, 2);
    CHK_ARG_CORE(sizearg.Get()->String() != NULL, 2);

    LispInt arity = InternalAsciiToInt(sizearg.Get()->String()->String());

    LispUserFunction* userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper.String()),arity);
    InternalBoolean(aEnvironment,RESULT,userFunc != NULL);
}

void LispDefLoadFunction(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(2);
    LispPtr name;
    name.Set(ARGUMENT(1).Get());
    LispStringPtr orig = name.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper.String()));
    if (multiUserFunc != NULL)
    {
        if (multiUserFunc->iFileToOpen!=NULL)
        {
            LispDefFile* def = multiUserFunc->iFileToOpen;
            if (!def->iIsLoaded)
            {
#ifdef YACAS_DEBUG
                /*Show loading... */
              {
                extern int verbose_debug;
                if (verbose_debug)
                  printf("Debug> Loading file %s for function %s\n",def->iFileName()->String(),oper.String());
              }
#endif
                multiUserFunc->iFileToOpen=NULL;
                InternalUse(aEnvironment,def->iFileName());
            }
        }
    }
    InternalTrue(aEnvironment,RESULT);
}


void LispRuleBaseArgList(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    //TESTARGS(3);
    LispPtr name;
    name.Set(ARGUMENT(1).Get());
    LispStringPtr orig = name.Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispPtr sizearg;
    sizearg.Set(ARGUMENT(2).Get());
    CHK_ARG_CORE(sizearg.Get() != NULL, 2);
    CHK_ARG_CORE(sizearg.Get()->String() != NULL, 2);

    LispInt arity = InternalAsciiToInt(sizearg.Get()->String()->String());

    LispUserFunction* userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper.String()),arity);
    CHK_CORE(userFunc != NULL, KLispErrInvalidArg);

    LispPtr& list = userFunc->ArgList();
    LispPtr head;
    head.Set(aEnvironment.iList->Copy(LispFalse));
    head.Get()->Next().Set(list.Get());
    RESULT.Set(LispSubList::New(head.Get()));
}


static void InternalNewRulePattern(LispEnvironment& aEnvironment, LispInt aStackTop, LispBoolean aMacroMode)
{
    //TESTARGS(6);

    LispInt arity;
    LispInt precedence;

    LispPtr ar;
    LispPtr pr;
    LispPtr predicate;
    LispPtr body;
    LispStringPtr orig=NULL;
    
    // Get operator
    CHK_ARG_CORE(ARGUMENT(1).Get() != NULL, 1);
    orig = ARGUMENT(1).Get()->String();
    CHK_ARG_CORE(orig != NULL, 1);
    ar.Set(ARGUMENT(2).Get());
    pr.Set(ARGUMENT(3).Get());
    predicate.Set(ARGUMENT(4).Get());
    body.Set(ARGUMENT(5).Get());
    
    // The arity
    CHK_ARG_CORE(ar.Get() != NULL, 2);
    CHK_ARG_CORE(ar.Get()->String() != NULL, 2);
    arity = InternalAsciiToInt(ar.Get()->String()->String());

    // The precedence
    CHK_ARG_CORE(pr.Get() != NULL, 3);
    CHK_ARG_CORE(pr.Get()->String() != NULL, 3);
    precedence = InternalAsciiToInt(pr.Get()->String()->String());
    
    // Finally define the rule base
    aEnvironment.DefineRulePattern(SymbolName(aEnvironment,orig->String()),
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



