
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
#include "anumber.h"
#include "arrayclass.h"
#include "patternclass.h"
#include "substitute.h"
#include "errors.h"
#include "arggetter.h"

#define InternalEval aEnvironment.iEvaluator->Eval


void LispLexCompare2(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments,
                     LispBoolean (*lexfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision),
#ifndef NO_USE_BIGFLOAT
                     LispBoolean (*numfunc)(BigNumber& n1, BigNumber& n2)
#else
                     LispBoolean (*numfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision)
#endif
                    );


void LispQuote(LispEnvironment& aEnvironment,
               LispPtr& aResult,
               LispPtr& aArguments)
{
    TESTARGS(2);
    aResult.Set(Argument(aArguments,1).Get()->Copy(LispFalse));
}

/*TODO remove? just an experiment, didn't pan out it seems
template<int T>
class CArgs
{
public:
    inline CArgs(LispPtr& aArguments,LispEnvironment& aEnvironment);
    inline LispPtr& Arg(LispInt aIndex);
    inline void Eval(LispInt aIndex);
private:
    LispEnvironment& iEnvironment;
    LispPtr *iArgs[T];
};
template<int T>
inline CArgs<T>::CArgs(LispPtr& aArguments,LispEnvironment& aEnvironment)
: iEnvironment(aEnvironment)
{
   LISPASSERT(aArguments.Get() != NULL);
   LispPtr* ptr = &aArguments.Get()->Next();
   LispInt i;
   for (i=0;i<T;i++)
   {
      iArgs[i] = ptr;
      ptr = &ptr->Get()->Next();
   }
   if (ptr->Get() != NULL)
   {
	ErrorNrArgs(T, InternalListLength(aArguments)-1, aArguments, aEnvironment);
   }
}

template<int T>
inline LispPtr& CArgs<T>::Arg(LispInt aIndex)
{
    LISPASSERT(aIndex >= 0 && aIndex < T);
    return *iArgs[aIndex];
}

 template<int T>
inline void CArgs<T>::Eval(LispInt aIndex)
{
   LispPtr result;
   iEnvironment.iEvaluator->Eval(iEnvironment, result, iArgs[aIndex]);
   iArgs[aIndex].Set(result.Get());
}
*/


void LispEval(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{
    /*TODO remove? just an experiment, didn't pan out it seems
    CArgs<1> args(aArguments,aEnvironment);
    LispPtr result;
    InternalEval(aEnvironment, result, args.Arg(0));
    InternalEval(aEnvironment, aResult, result);
    */

    TESTARGS(2);
    LispPtr result;
    InternalEval(aEnvironment, result, Argument(aArguments,1));
    InternalEval(aEnvironment, aResult, result);
}

static void InternalSetVar(LispEnvironment& aEnvironment, LispPtr& aResult,
                LispPtr& aArguments,LispBoolean aMacroMode)
{
    TESTARGS(3);

    LispStringPtr varstring=NULL;

    if (aMacroMode)
    {
        LispPtr result;
        InternalEval(aEnvironment, result, Argument(aArguments,1));
        varstring = result.Get()->String();
    }
    else
    {
        varstring = Argument(aArguments,1).Get()->String();
    }
    CHK_ARG(varstring != NULL,1);
    CHK_ARG(!IsNumber(varstring->String(),LispTrue),1);
    
    LispPtr result;
    InternalEval(aEnvironment, result, Argument(aArguments,2));
    aEnvironment.SetVariable(varstring, result);
    InternalTrue(aEnvironment,aResult);
}


void LispSetVar(LispEnvironment& aEnvironment, LispPtr& aResult,
                LispPtr& aArguments)
{
    InternalSetVar(aEnvironment, aResult,aArguments,LispFalse);
}
void LispMacroSetVar(LispEnvironment& aEnvironment, LispPtr& aResult,
                LispPtr& aArguments)
{
    InternalSetVar(aEnvironment, aResult,aArguments,LispTrue);
}


static void InternalClearVar(LispEnvironment& aEnvironment,
                      LispPtr& aResult, LispPtr& aArguments,
                      LispBoolean aMacroMode)
{
    LispIterator iter(Argument(aArguments,1));
    LispInt nr=1;
    while (iter())
    {
        LispStringPtr str;

        if (aMacroMode)
        {
            LispPtr result;
            InternalEval(aEnvironment, result, *iter.Ptr());
            str = result.Get()->String();
        }
        else
        {
            str = iter()->String();
        }

        CHK_ARG(str != NULL, nr);
        aEnvironment.UnsetVariable(str);
        iter.GoNext();
        nr++;
    }
    InternalTrue(aEnvironment,aResult);
}

void LispClearVar(LispEnvironment& aEnvironment,
                  LispPtr& aResult,LispPtr& aArguments)
{
    InternalClearVar(aEnvironment,aResult, aArguments, LispFalse);
}
void LispMacroClearVar(LispEnvironment& aEnvironment,
                  LispPtr& aResult,LispPtr& aArguments)
{
    InternalClearVar(aEnvironment,aResult, aArguments, LispTrue);
}



/* StrCompare returns f1-f2: if f1 < f2 it returns -1, if f1=f2 it
 returns 0, and it returns 1 if f1>f2
 */
static LispBoolean LexLessThan(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision)
{
    return (StrCompare(f1, f2)<0);
}

static LispBoolean LexGreaterThan(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision)
{
    return (StrCompare(f1, f2)>0);
}

#ifndef NO_USE_BIGFLOAT
static LispBoolean BigLessThan(BigNumber& n1, BigNumber& n2)
{
LispString str;
n1.ToString(str,10,10);
n2.ToString(str,10,10);

  return n1.LessThan(n2);
}
static LispBoolean BigGreaterThan(BigNumber& n1, BigNumber& n2)
{
  return !(n1.LessThan(n2) || n1.Equals(n2));
}
#endif

void LispLessThan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
#ifndef NO_USE_BIGFLOAT
    LispLexCompare2(aEnvironment, aResult, aArguments, LexLessThan,BigLessThan);
#else
    LispLexCompare2(aEnvironment, aResult, aArguments, LexLessThan,LessThan);
#endif
}

void LispGreaterThan(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
#ifndef NO_USE_BIGFLOAT
    LispLexCompare2(aEnvironment, aResult, aArguments, LexGreaterThan, BigGreaterThan);
#else
    LispLexCompare2(aEnvironment, aResult, aArguments, LexGreaterThan, GreaterThan);
#endif
}


void LispLexCompare2(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments,
                     LispBoolean (*lexfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision),
#ifndef NO_USE_BIGFLOAT
                     LispBoolean (*numfunc)(BigNumber& n1, BigNumber& n2)
#else
                     LispBoolean (*numfunc)(LispCharPtr f1, LispCharPtr f2, LispHashTable& aHashTable,LispInt aPrecision)
#endif
                    )
{
    TESTARGS(3);
    LispPtr result1;
    LispPtr result2;
    InternalEval(aEnvironment, result1, Argument(aArguments,1));
    InternalEval(aEnvironment, result2, Argument(aArguments,2));
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
    CHK_ARG(str1 != NULL ,1);
    CHK_ARG(str2 != NULL, 2);
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
      CHK_ARG(str1 != NULL ,1);
      CHK_ARG(str2 != NULL, 2);
#endif
      cmp =lexfunc(str1->String(),str2->String(),
                            aEnvironment.HashTable(),
                            aEnvironment.Precision());
    }
    
    InternalBoolean(aEnvironment,aResult, cmp);
}

void LispPi(LispEnvironment& aEnvironment, LispPtr& aResult,
            LispPtr& aArguments)
{
    TESTARGS(1);
    aResult.Set(LispAtom::New(aEnvironment,PiFloat(aEnvironment.HashTable(),
                                         aEnvironment.Precision())));
}



void LispGcd(LispEnvironment& aEnvironment, LispPtr& aResult,
             LispPtr& aArguments)
{
    LispArgGetter g(aEnvironment, aArguments);
    IntegerArgument(g,str1,LispTrue);
    IntegerArgument(g,str2,LispTrue);
    g.Finalize(2);
    aResult.Set(LispAtom::New(aEnvironment,GcdInteger(str1->String(),str2->String(),
                                         aEnvironment.HashTable())));
}

void LispFullForm(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalEval(aEnvironment, aResult, Argument(aArguments,1));
    LispPrinter printer;
    printer.Print(aResult, *aEnvironment.CurrentOutput(), aEnvironment);
    aEnvironment.CurrentOutput()->Write("\n");
}


void LispHead(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArgGetter g(aEnvironment, aArguments);
    ListArgument(g,list,LispTrue);
    g.Finalize(1);
    InternalNth(aResult, list,1);
}

void LispNth(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArgGetter g(aEnvironment, aArguments);
    ListArgument(g,list,LispTrue);
    ShortIntegerArgument(g,index,LispTrue);
    g.Finalize(2);
    InternalNth(aResult, list, index);
}


void LispTail(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArgGetter g(aEnvironment, aArguments);
    ListArgument(g,list,LispTrue);
    g.Finalize(1);

    LispPtr first;
    InternalTail(first, list);
    InternalTail(aResult, first);
    LispPtr head;
    head.Set(LispAtom::New(aEnvironment,aEnvironment.iList));
    head.Get()->Next().Set(aResult.Get()->SubList()->Get());
    aResult.Get()->SubList()->Set(head.Get());
}

void LispUnList(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
  LispArgGetter g(aEnvironment, aArguments);
  ListArgument(g,lst,LispTrue);
  g.Finalize(1);
  if (lst.Get() != NULL)
  {
    LispObject* subList = lst.Get()->SubList()->Get();
    if (subList)
      if (subList->String() == aEnvironment.iList)
      {
        InternalTail(aResult, lst);
        return;
      }
  }
  CHK_ARG(LispFalse, 1);
  
//  InternalTail(aResult, list);
}

void LispListify(LispEnvironment& aEnvironment, LispPtr& aResult,
                 LispPtr& aArguments)
{
    LispArgGetter g(aEnvironment, aArguments);
    ListArgument(g,list,LispTrue);
    g.Finalize(1);

    LispPtr head;
    head.Set(LispAtom::New(aEnvironment,aEnvironment.iList));
    head.Get()->Next().Set(list.Get()->SubList()->Get());
    aResult.Set(LispSubList::New(head.Get()));
}




void LispDestructiveReverse(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispPtr& aArguments)
{
    LispArgGetter g(aEnvironment, aArguments);
    ListArgument(g,list,LispTrue);
    g.Finalize(1);

    LispPtr reversed;
    reversed.Set(LispAtom::New(aEnvironment,aEnvironment.iList));
    InternalReverseList(reversed.Get()->Next(), list.Get()->SubList()->Get()->Next());
    aResult.Set(LispSubList::New(reversed.Get()));
}


void LispLength(LispEnvironment& aEnvironment, LispPtr& aResult,
                LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    LispPtr* subList = evaluated.Get()->SubList();
    if (subList != NULL)
    {
        LispChar s[20];
        LispInt num = InternalListLength(subList->Get()->Next());
        InternalIntToAscii(s,num);
        aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(s)));
        return;
    }
    LispStringPtr string = evaluated.Get()->String();
    if (InternalIsString(string))
    {
        LispChar s[20];
        LispInt num = string->NrItems()-3;
        InternalIntToAscii(s,num);
        aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(s)));
        return;
    }
    GenericClass *gen = evaluated.Get()->Generic();
    if (gen != NULL)
    if (StrEqual(gen->TypeName(),"\"Array\""))
    {
        LispInt size=((ArrayClass*)gen)->Size();
        LispChar s[20];
        InternalIntToAscii(s,size);
        aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(s)));
        return;
    }
    CHK_ISLIST(evaluated,1);
}


void LispList(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    LispPtr all;
    all.Set(LispAtom::New(aEnvironment,aEnvironment.iList));
    LispIterator tail(all);
    tail.GoNext();
    LispIterator iter = Argument(aArguments,1);
    while (iter())
    {
        LispPtr evaluated;
        InternalEval(aEnvironment, evaluated, *iter.Ptr());
        tail.Ptr()->Set(evaluated.Get());
        tail.GoNext();
        iter.GoNext();
    }
    
    aResult.Set(LispSubList::New(all.Get()));
}


void LispConcatenate(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    LispPtr all;
    all.Set(LispAtom::New(aEnvironment,aEnvironment.iList));
    LispIterator tail(all);
    tail.GoNext();
    LispInt arg = 1;

    LispIterator iter = Argument(aArguments,1);
    while (iter())
    {
        LispPtr evaluated;
        InternalEval(aEnvironment, evaluated, *iter.Ptr());
        CHK_ISLIST(evaluated,arg);
        InternalFlatCopy(*tail.Ptr(),evaluated.Get()->SubList()->Get()->Next());
        while (tail() != NULL)
            tail.GoNext();

        iter.GoNext();
        arg++;
    }
    
    aResult.Set(LispSubList::New(all.Get()));
}


static void ConcatenateStrings(LispStringSmartPtr& aSmartPtr, LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    LISPASSERT(aSmartPtr());
    aSmartPtr()->SetNrItems(0);
    aSmartPtr()->Append('\"');
    LispInt arg=1;
    
    LispIterator iter = Argument(aArguments,1);
    while (iter())
    {
        LispPtr evaluated;
        InternalEval(aEnvironment, evaluated, *iter.Ptr());
        CHK_ISSTRING(evaluated,arg);

        LispInt length = evaluated.Get()->String()->NrItems()-2;
        LispCharPtr ptr=evaluated.Get()->String()->String();
        
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
void LispConcatenateStrings(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
{
    LispString *str = NEW LispString;
    LispStringSmartPtr smartptr;
    smartptr.Set(str);
    ConcatenateStrings(smartptr,aEnvironment, aResult, aArguments);
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(str)));
}

static void InternalDelete(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments, LispInt aDestructive)
{
    TESTARGS(3);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));
    CHK_ISLIST(evaluated,1);

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
    InternalEval(aEnvironment, index, Argument(aArguments,2));
    CHK_ARG(index.Get() != NULL, 2);
    CHK_ARG(index.Get()->String() != NULL, 2);
    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());
    CHK_ARG(ind>0,2);

    LispIterator iter(copied);
    while (ind>0)
    {
        iter.GoNext();
        ind--;
    }
    CHK(iter() != NULL, KLispErrListNotLongEnough);
    LispPtr next;
    next.Set(iter()->Next().Get());
    iter.Ptr()->Set(next.Get());
    aResult.Set(LispSubList::New(copied.Get()));
}
void LispDelete(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalDelete(aEnvironment, aResult,aArguments,LispFalse);
}
void LispDestructiveDelete(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
    InternalDelete(aEnvironment, aResult,aArguments,LispTrue);
}

void LispFlatCopy(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispArgGetter g(aEnvironment, aArguments);
    ListArgument(g,list,LispTrue);
    g.Finalize(1);
    LispPtr copied;
    InternalFlatCopy(copied,*list.Get()->SubList());
    aResult.Set(LispSubList::New(copied.Get()));
}


static void InternalInsert(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments, LispInt aDestructive)
{
    TESTARGS(4);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));
    CHK_ISLIST(evaluated,1);

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
    InternalEval(aEnvironment, index, Argument(aArguments,2));
    CHK_ARG(index.Get() != NULL, 2);
    CHK_ARG(index.Get()->String() != NULL, 2);
    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());
    CHK_ARG(ind>0,2);

    LispIterator iter(copied);
    while (ind>0)
    {
        iter.GoNext();
        ind--;
    }

    LispPtr toInsert;
    InternalEval(aEnvironment, toInsert, Argument(aArguments,3));
    toInsert.Get()->Next().Set(iter());
    iter.Ptr()->Set(toInsert.Get());
    aResult.Set(LispSubList::New(copied.Get()));
}

void LispInsert(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalInsert(aEnvironment, aResult,aArguments,LispFalse);
}

void LispDestructiveInsert(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalInsert(aEnvironment, aResult,aArguments,LispTrue);
}







static void InternalReplace(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments, LispInt aDestructive)
{
    TESTARGS(4);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));
    CHK_ISLIST(evaluated,1);

    LispPtr index;
    InternalEval(aEnvironment, index, Argument(aArguments,2));
    CHK_ARG(index.Get() != NULL, 2);
    CHK_ARG(index.Get()->String() != NULL, 2);
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
    CHK_ARG(ind>0,2);

    LispIterator iter(copied);
    while (ind>0)
    {
        iter.GoNext();
        ind--;
    }

    LispPtr toInsert;
    InternalEval(aEnvironment, toInsert, Argument(aArguments,3));
    CHK_ARG(iter.Ptr() != NULL, 2);
    CHK_ARG(iter.Ptr()->Get() != NULL, 2);
    toInsert.Get()->Next().Set(iter.Ptr()->Get()->Next().Get());
    iter.Ptr()->Set(toInsert.Get());
    aResult.Set(LispSubList::New(copied.Get()));
}

void LispReplace(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalReplace(aEnvironment, aResult,aArguments,LispFalse);
}

void LispDestructiveReplace(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalReplace(aEnvironment, aResult,aArguments,LispTrue);
}
















void LispNot(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));
    if (IsTrue(aEnvironment, evaluated) || IsFalse(aEnvironment, evaluated))
    {
        InternalNot(aResult, aEnvironment, evaluated);
    }
    else
    {
        LispPtr ptr;
        ptr.Set(aArguments.Get()->Copy(LispFalse));
        ptr.Get()->Next().Set(evaluated.Get());
        aResult.Set(LispSubList::New(ptr.Get()));
    }
}

void LispLazyAnd(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispPtr nogos;
    LispInt nrnogos=0;
    LispPtr evaluated;

    LispIterator iter(Argument(aArguments,1));
    while (iter())
    {
        InternalEval(aEnvironment, evaluated, *iter.Ptr());
        if (IsFalse(aEnvironment, evaluated))
        {
            InternalFalse(aEnvironment,aResult);
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
            aResult.Set(nogos.Get());
        }
        else
        {
            LispPtr ptr;

            InternalReverseList(ptr, nogos);
            nogos.Set(ptr.Get());

            ptr.Set(aArguments.Get()->Copy(LispFalse));
            ptr.Get()->Next().Set(nogos.Get());
            nogos.Set(ptr.Get());
            aResult.Set(LispSubList::New(nogos.Get()));

            //aEnvironment.CurrentPrinter().Print(aResult, *aEnvironment.CurrentOutput());
        }
    }
    else
    {
        InternalTrue(aEnvironment,aResult);
    }
}

void LispLazyOr(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispPtr nogos;
    LispInt nrnogos=0;

    LispPtr evaluated;

    LispIterator iter(Argument(aArguments,1));
    while (iter())
    {
        InternalEval(aEnvironment, evaluated, *iter.Ptr());
        if (IsTrue(aEnvironment, evaluated))
        {
            InternalTrue(aEnvironment,aResult);
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
            aResult.Set(nogos.Get());
        }
        else
        {
            LispPtr ptr;

            InternalReverseList(ptr, nogos);
            nogos.Set(ptr.Get());

            ptr.Set(aArguments.Get()->Copy(LispFalse));
            ptr.Get()->Next().Set(nogos.Get());
            nogos.Set(ptr.Get());
            aResult.Set(LispSubList::New(nogos.Get()));
        }
        //aEnvironment.CurrentPrinter().Print(aResult, *aEnvironment.CurrentOutput());
    }
    else
    {
        InternalFalse(aEnvironment,aResult);
    }
}

void LispEquals(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(3);
    LispPtr evaluated1;
    InternalEval(aEnvironment, evaluated1, Argument(aArguments,1));
    LispPtr evaluated2;
    InternalEval(aEnvironment, evaluated2, Argument(aArguments,2));

    InternalBoolean(aEnvironment,aResult,
                    InternalEquals(aEnvironment, evaluated1, evaluated2));
}


void LispWrite(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispPtr evaluated;
    LispInt nrArguments = InternalListLength(aArguments);
    LispInt arg;
    for (arg=1;arg<nrArguments;arg++)
    {
        InternalEval(aEnvironment, evaluated, Argument(aArguments,arg));
        aEnvironment.CurrentPrinter().Print(evaluated,
                                            *aEnvironment.CurrentOutput(),
                                           aEnvironment);
    }
    InternalTrue(aEnvironment,aResult);
}

void LispWriteString(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));
    CHK_ARG(evaluated.Get()!= NULL,1);
    LispStringPtr str = evaluated.Get()->String();
    CHK_ARG(str != NULL,1);
    CHK_ARG((*str)[0] == '\"',1);
    CHK_ARG((*str)[str->NrItems()-2] == '\"',1);

    LispInt i=1;
    LispInt nr=str->NrItems()-2;
    //((*str)[i] != '\"')
    for (i=1;i<nr;i++)
    {
        aEnvironment.CurrentOutput()->PutChar((*str)[i]);
    }
	// pass last printed character to the current printer
	aEnvironment.CurrentPrinter().RememberLastChar((*str)[nr-1]);	// hacky hacky
    InternalTrue(aEnvironment,aResult);
}

void LispProgBody(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    // Allow accessing previous locals.
    LispLocalFrame frame(aEnvironment,LispFalse);

    InternalTrue(aEnvironment,aResult);
    
    // Evaluate args one by one.
    LispInt nrArguments = InternalListLength(aArguments);
    LispInt arg;
    for (arg=1;arg<nrArguments;arg++)
    {
        InternalEval(aEnvironment, aResult, Argument(aArguments,arg));
    }
}

static void InternalNewLocal(LispEnvironment& aEnvironment, LispPtr& aResult,
                             LispPtr& aArguments, LispBoolean aMacroMode)
{
    LispInt nrArguments = InternalListLength(aArguments);
    LispInt arg;
    for (arg=1;arg<nrArguments;arg++)
    {
        LispStringPtr variable=NULL;
        if (aMacroMode)
        {
            LispPtr result;
            InternalEval(aEnvironment, result,  Argument(aArguments,arg));
            variable = result.Get()->String();
        }
        else
        {
            variable = Argument(aArguments,arg).Get()->String();
        }
        CHK_ARG(variable != NULL,arg);
        aEnvironment.NewLocal(variable,NULL);
    }
    InternalTrue(aEnvironment,aResult);
}

void LispNewLocal(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalNewLocal(aEnvironment, aResult,aArguments, LispFalse);
}

void LispMacroNewLocal(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments)
{
    InternalNewLocal(aEnvironment, aResult,aArguments, LispTrue);
}


void LispWhile(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(3);

    LispPtr& arg1 = Argument(aArguments,1);
    LispPtr& arg2 = Argument(aArguments,2);
    
    LispPtr predicate;
    InternalEval(aEnvironment, predicate, arg1);

    while (IsTrue(aEnvironment,predicate))
    {
        LispPtr evaluated;
        InternalEval(aEnvironment, evaluated, arg2);
        InternalEval(aEnvironment, predicate, arg1);

    }
    CHK_ARG(IsFalse(aEnvironment,predicate),1);
    InternalTrue(aEnvironment,aResult);
}




static void MultiFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aArguments, LispOperators& aOps)
{
    TESTARGS(3);

    // Get operator
    CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
    LispStringPtr orig = Argument(aArguments,1).Get()->String();
    CHK_ARG(orig != NULL, 1);
    
    LispPtr precedence;
    InternalEval(aEnvironment, precedence, Argument(aArguments,2));
    CHK_ARG(precedence.Get()->String() != NULL, 2);
    LispInt prec = InternalAsciiToInt(precedence.Get()->String()->String());
    CHK_ARG(prec <= KMaxPrecedence, 2);
    aOps.SetOperator(prec,SymbolName(aEnvironment,orig->String()));
    InternalTrue(aEnvironment,aResult);
}

void LispInFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    MultiFix(aEnvironment, aResult, aArguments, aEnvironment.InFix());
}


static void SingleFix(LispInt aPrecedence, LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments, LispOperators& aOps)
{
    TESTARGS(2);

    // Get operator
    CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
    LispStringPtr orig = Argument(aArguments,1).Get()->String();
    CHK_ARG(orig != NULL, 1);
    aOps.SetOperator(aPrecedence,SymbolName(aEnvironment,orig->String()));
    InternalTrue(aEnvironment,aResult);
}
void LispPreFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispInt nrArguments = InternalListLength(aArguments);
    if (nrArguments == 2)
    {
        SingleFix(0, aEnvironment, aResult,aArguments, aEnvironment.PreFix());
    }
    else
    {
        MultiFix(aEnvironment, aResult, aArguments, aEnvironment.PreFix());
    }
}
void LispPostFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispInt nrArguments = InternalListLength(aArguments);
    if (nrArguments == 2)
    {
        SingleFix(0, aEnvironment, aResult,aArguments, aEnvironment.PostFix());
    }
    else
    {
        MultiFix(aEnvironment, aResult, aArguments, aEnvironment.PostFix());
    }
//    SingleFix(0, aEnvironment, aResult,aArguments, aEnvironment.PostFix());
}
void LispBodied(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    MultiFix(aEnvironment, aResult, aArguments, aEnvironment.Bodied());
}


void LispAtomize(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get operator
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpUnStringify(orig->String())));
}


void LispStringify(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get operator
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);

    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(orig->String())));
}




void LispLoad(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(2);
    CHK(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get file name
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);

    InternalLoad(aEnvironment,orig);
    InternalTrue(aEnvironment,aResult);
}


static void InternalRuleBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                             LispPtr& aArguments, LispBoolean aMacroMode,
                             LispInt aListed)
{
    TESTARGS(3);
    
    // Get operator
    LispPtr args;
    LispStringPtr orig=NULL;
    
    if (aMacroMode)
    {
        LispPtr result;
        InternalEval(aEnvironment, result, Argument(aArguments,1));
        orig = result.Get()->String();
        CHK_ARG(orig != NULL, 1);
        InternalEval(aEnvironment, args, Argument(aArguments,2));
    }
    else
    {
        CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
        orig = Argument(aArguments,1).Get()->String();
        CHK_ARG(orig != NULL, 1);
        args.Set(Argument(aArguments,2).Get());
    }
    
    // The arguments
    CHK_ISLIST(args,2);

    // Finally define the rule base
    aEnvironment.DeclareRuleBase(SymbolName(aEnvironment,orig->String()),
                                 args.Get()->SubList()->Get()->Next(),aListed);
    
    // Return LispTrue
    InternalTrue(aEnvironment,aResult);
}

void LispRuleBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalRuleBase(aEnvironment, aResult, aArguments, LispFalse,LispFalse);
}
void LispMacroRuleBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments)
{
    InternalRuleBase(aEnvironment, aResult, aArguments, LispTrue,LispFalse);
}

void InternalDefMacroRuleBase(LispEnvironment& aEnvironment, LispPtr& aResult,
                      LispPtr& aArguments, LispInt aListed)

{
    TESTARGS(3);
    
    // Get operator
    LispPtr args;
    LispPtr body;
    LispStringPtr orig=NULL;
    
    CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
    orig = Argument(aArguments,1).Get()->String();
    CHK_ARG(orig != NULL, 1);

    // The arguments
    args.Set(Argument(aArguments,2).Get());
    CHK_ISLIST(args,2);

    // Finally define the rule base
    aEnvironment.DeclareMacroRuleBase(SymbolName(aEnvironment,orig->String()),
                                 args.Get()->SubList()->Get()->Next(),aListed);
    
    // Return LispTrue
    InternalTrue(aEnvironment,aResult);

}

void LispDefMacroRuleBaseListed(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments)
{
  InternalDefMacroRuleBase(aEnvironment, aResult, aArguments, LispTrue);
}
void LispDefMacroRuleBase(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aArguments)
{
  InternalDefMacroRuleBase(aEnvironment, aResult, aArguments, LispFalse);
}


void LispRuleBaseListed(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalRuleBase(aEnvironment, aResult, aArguments, LispFalse,LispTrue);
}
void LispMacroRuleBaseListed(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments)
{
    InternalRuleBase(aEnvironment, aResult, aArguments, LispTrue,LispTrue);
}


void LispHoldArg(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(3);
    
    // Get operator
    CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
    LispStringPtr orig = Argument(aArguments,1).Get()->String();
    CHK_ARG(orig != NULL, 1);

    // The arguments
    LispStringPtr tohold = Argument(aArguments,2).Get()->String();
    CHK_ARG(tohold != NULL, 2);
    aEnvironment.HoldArgument(SymbolName(aEnvironment,orig->String()), tohold);
    // Return LispTrue
    InternalTrue(aEnvironment,aResult);
}

static void InternalNewRule(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispPtr& aArguments, LispBoolean aMacroMode)
{
    TESTARGS(6);

    LispInt arity;
    LispInt precedence;

    LispPtr ar;
    LispPtr pr;
    LispPtr predicate;
    LispPtr body;
    LispStringPtr orig=NULL;
    
    // Get operator
    if (aMacroMode)
    {
        LispPtr result;
        InternalEval(aEnvironment, result, Argument(aArguments,1));
        CHK_ARG(result.Get() != NULL, 1);
        orig = result.Get()->String();
        CHK_ARG(orig != NULL, 1);

        InternalEval(aEnvironment, ar, Argument(aArguments,2));
        InternalEval(aEnvironment, pr, Argument(aArguments,3));
        InternalEval(aEnvironment, predicate, Argument(aArguments,4));
        InternalEval(aEnvironment, body, Argument(aArguments,5));
    }
    else
    {
        CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
        orig = Argument(aArguments,1).Get()->String();
        CHK_ARG(orig != NULL, 1);
        ar.Set(Argument(aArguments,2).Get());
        pr.Set(Argument(aArguments,3).Get());
        predicate.Set(Argument(aArguments,4).Get());
        body.Set(Argument(aArguments,5).Get());
    }
    
    // The arity
    CHK_ARG(ar.Get() != NULL, 2);
    CHK_ARG(ar.Get()->String() != NULL, 2);
    arity = InternalAsciiToInt(ar.Get()->String()->String());

    // The precedence
    CHK_ARG(pr.Get() != NULL, 3);
    CHK_ARG(pr.Get()->String() != NULL, 3);
    precedence = InternalAsciiToInt(pr.Get()->String()->String());
    
    // Finally define the rule base
    aEnvironment.DefineRule(SymbolName(aEnvironment,orig->String()),
                            arity,
                            precedence,
                            predicate,
                            body );

    // Return LispTrue
    InternalTrue(aEnvironment,aResult);
}

void LispNewRule(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalNewRule(aEnvironment, aResult,aArguments, LispFalse);
}

void LispMacroNewRule(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalNewRule(aEnvironment, aResult,aArguments, LispTrue);
}


void LispUnFence(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(3);
    
    // Get operator
    CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
    LispStringPtr orig = Argument(aArguments,1).Get()->String();
    CHK_ARG(orig != NULL, 1);

    // The arity
    CHK_ARG(Argument(aArguments,2).Get() != NULL, 2);
    CHK_ARG(Argument(aArguments,2).Get()->String() != NULL, 2);
    LispInt arity = InternalAsciiToInt(Argument(aArguments,2).Get()->String()->String());

    aEnvironment.UnFenceRule(SymbolName(aEnvironment,orig->String()),
                            arity);
    
    // Return LispTrue
    InternalTrue(aEnvironment,aResult);
}


void LispMathLibName(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{
	TESTARGS(1);
        // can't use NumericLibraryName() inside LookUpStringify() b/c of
        // nonconstant pointer! why is it not a const char* and do I have to
        // write this?
        // const_cast removes const-ness... ;-)
        char* library_name = const_cast<char*>(NumericLibraryName());
        aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(
		library_name
	)));
}

void LispIsFunction(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr result;
    InternalEval(aEnvironment, result, Argument(aArguments,1));
    InternalBoolean(aEnvironment,aResult,
                    result.Get()->SubList()!=NULL);
}
void LispIsAtom(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr result;
    InternalEval(aEnvironment, result, Argument(aArguments,1));
    InternalBoolean(aEnvironment,aResult,
                    result.Get()->String()!=NULL);
}
void LispIsNumber(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr result;
    InternalEval(aEnvironment, result, Argument(aArguments,1));

#ifndef NO_USE_BIGFLOAT
/*
//    if (result.Get() == 0)
    {
      LispString str;
      PrintExpression(str, result, aEnvironment, 80);
      printf("%s\n",str.String());
    }
*/
    if (result.Get()->Number(aEnvironment.Precision()) == NULL)
    {
        InternalFalse(aEnvironment,aResult);
    }
    else
    {
        InternalTrue(aEnvironment,aResult);
    }
#else
    if (result.Get()->String() == NULL)
    {
        InternalFalse(aEnvironment,aResult);
    }
    else
    {
        InternalBoolean(aEnvironment,aResult,
                        IsNumber(result.Get()->String()->String(),LispTrue));
    }
#endif
}

void LispIsInteger(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr result;
    InternalEval(aEnvironment, result, Argument(aArguments,1));

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
        InternalFalse(aEnvironment,aResult);
    }
    else if (!num->IsInt())
    {
        InternalFalse(aEnvironment,aResult);
    }
    else
    {
        InternalTrue(aEnvironment,aResult);
    }
#else
    if (result.Get()->String() == NULL)
    {
        InternalFalse(aEnvironment,aResult);
    }
    else
    {
        InternalBoolean(aEnvironment,aResult,
                        IsNumber(result.Get()->String()->String(),LispFalse));
    }
#endif
}


void LispIsList(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr result;
    InternalEval(aEnvironment, result, Argument(aArguments,1));
    InternalBoolean(aEnvironment,aResult,InternalIsList(result));
}


void LispIsString(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr result;
    InternalEval(aEnvironment, result, Argument(aArguments,1));
    InternalBoolean(aEnvironment,aResult,
                    InternalIsString(result.Get()->String()));
}

void LispIsBound(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispStringPtr str = Argument(aArguments,1).Get()->String();
    if (str)
    {
        LispPtr val;
        aEnvironment.GetVariable(str,val);
        if (val.Get())
        {
            InternalTrue(aEnvironment,aResult);
            return;
        }
    }
    InternalFalse(aEnvironment,aResult);
}



void LispIf(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispInt nrArguments = InternalListLength(aArguments);
    CHK(nrArguments == 3 || nrArguments == 4,KLispErrWrongNumberOfArgs);

    LispPtr predicate;
    InternalEval(aEnvironment, predicate, Argument(aArguments,1));

    if (IsTrue(aEnvironment,predicate))
    {
        InternalEval(aEnvironment, aResult, Argument(aArguments,2));
    }
    else
    {
        CHK_ARG(IsFalse(aEnvironment,predicate),1);
        if (nrArguments == 4)
        {
            InternalEval(aEnvironment, aResult, Argument(aArguments,3));
        }
        else
        {
            InternalFalse(aEnvironment,aResult);
        }
    }
}



void LispRetract(LispEnvironment& aEnvironment, LispPtr& aResult,
                 LispPtr& aArguments)
{
    TESTARGS(3);

    // Get operator
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);
    LispStringPtr oper = SymbolName(aEnvironment,orig->String());
    
    LispPtr arity;
    InternalEval(aEnvironment, arity, Argument(aArguments,2));
    CHK_ARG(arity.Get()->String() != NULL, 2);
    LispInt ar = InternalAsciiToInt(arity.Get()->String()->String());
    aEnvironment.Retract(oper, ar);
    InternalTrue(aEnvironment,aResult);
}


void LispPrecision(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr index;
    InternalEval(aEnvironment, index, Argument(aArguments,1));
    CHK_ARG(index.Get() != NULL, 1);
    CHK_ARG(index.Get()->String() != NULL, 1);

    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());
    CHK_ARG(ind>0,1);
    aEnvironment.SetPrecision(ind);
    InternalTrue(aEnvironment,aResult);
}



void LispDefaultDirectory(LispEnvironment& aEnvironment, LispPtr& aResult,
                          LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get file name
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);
    aEnvironment.iInputDirectories.Append(NEW LispString(oper.String()));
    InternalTrue(aEnvironment,aResult);
}


void LispFromFile(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(3);

    CHK(aEnvironment.iSecure == 0, KLispErrSecurityBreach);
    
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get file name
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);

    LispStringPtr contents = aEnvironment.FindCachedFile(orig->String());
    LispStringPtr hashedname = aEnvironment.HashTable().LookUpUnStringify(orig->String());

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname->String());

    if (contents)
    {
        StringInput newInput(*contents,aEnvironment.iInputStatus);
        LispLocalInput localInput(aEnvironment, &newInput);

        // Evaluate the body
        InternalEval(aEnvironment, aResult, Argument(aArguments,2));
        delete contents;
    }
    else
    {
        //TODO make the file api platform independent!!!!
        // Open file
        LispLocalFile localFP(aEnvironment, hashedname->String(),LispTrue,
                              aEnvironment.iInputDirectories);
        CHK(localFP.iOpened != 0, KLispErrFileNotFound);
        FILEINPUT newInput(localFP,aEnvironment.iInputStatus);
        LispLocalInput localInput(aEnvironment, &newInput);

        // Evaluate the body
        InternalEval(aEnvironment, aResult, Argument(aArguments,2));
    }
    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
    //Return the result
}


void LispFromString(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(3);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get file name
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo("String");
    StringInput newInput(oper,aEnvironment.iInputStatus);
    LispLocalInput localInput(aEnvironment, &newInput);

    // Evaluate the body
    InternalEval(aEnvironment, aResult, Argument(aArguments,2));
    aEnvironment.iInputStatus.RestoreFrom(oldstatus);

    //Return the result
}


void LispRead(LispEnvironment& aEnvironment, LispPtr& aResult,
              LispPtr& aArguments)
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
    parser.Parse(aResult);
}


void LispReadToken(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{
    LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
    LispStringPtr result;
    result = tok.NextToken(*aEnvironment.CurrentInput(),
                           aEnvironment.HashTable());

    if (result->String()[0] == '\0')
    {
        aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp("EndOfFile")));
        return;
    }
    aResult.Set(LispAtom::New(aEnvironment,result));
}


void LispToFile(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(3);
    CHK(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get file name
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    //TODO make the file api platform independent!!!!
    // Open file for writing
    LispLocalFile localFP(aEnvironment, oper.String(),LispFalse,
                          aEnvironment.iInputDirectories);
    CHK(localFP.iOpened != 0, KLispErrFileNotFound);
    StdFileOutput newOutput(localFP);
    LispLocalOutput localOutput(aEnvironment, &newOutput);

    // Evaluate the body
    InternalEval(aEnvironment, aResult, Argument(aArguments,2));

    //Return the result
}



void LispCheck(LispEnvironment& aEnvironment,LispPtr& aResult,
               LispPtr& aArguments)
{
    TESTARGS(3);

    InternalEval(aEnvironment, aResult, Argument(aArguments,1));
    if (!IsTrue(aEnvironment,aResult))
    {
        LispPtr evaluated;
        InternalEval(aEnvironment, evaluated, Argument(aArguments,2));
        CHK_ISSTRING(evaluated,2);
        aEnvironment.SetUserError(evaluated.Get()->String()->String());
        CHK(0,KLispErrUser);
    }
}



void LispSystemCall(LispEnvironment& aEnvironment,LispPtr& aResult,
               LispPtr& aArguments)
{
    TESTARGS(2);
    CHK(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr result;
    InternalEval(aEnvironment, result, Argument(aArguments,1));
    CHK_ISSTRING(result,1);

    LispString command;
    InternalUnstringify(command, result.Get()->String());

// we would like to pass the exit code back to Yacas. Right now, let's pass True/False according to whether the exit code is 0 or not.
#ifdef SystemCall
	if(SystemCall(command.String()) == 0)
	{	
	    InternalTrue(aEnvironment,aResult);
	}
	else
	{
	    InternalFalse(aEnvironment,aResult);
	}
#else
    InternalFalse(aEnvironment,aResult);
#endif
}






void LispFastPi(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(1);
    aResult.Set(LispAtom::New(aEnvironment,PlatPi(aEnvironment.HashTable(),
                                         aEnvironment.Precision())));
}













void LispMaxEvalDepth(LispEnvironment& aEnvironment, LispPtr& aResult,
                      LispPtr& aArguments)
{
    TESTARGS(2);

    LispPtr index;
    InternalEval(aEnvironment, index, Argument(aArguments,1));
    CHK_ARG(index.Get() != NULL, 1);
    CHK_ARG(index.Get()->String() != NULL, 1);

    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());
    aEnvironment.iMaxEvalDepth = ind;
    InternalTrue(aEnvironment,aResult);
}


void LispDefLoad(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(2);
    CHK(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get file name
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);

    LoadDefFile(aEnvironment, orig);
    InternalTrue(aEnvironment,aResult);
}

void LispUse(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(2);
//This one seems safe...    CHK(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get file name
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);

    InternalUse(aEnvironment,orig);
    InternalTrue(aEnvironment,aResult);
}

void LispRightAssociative(LispEnvironment& aEnvironment, LispPtr& aResult,
                          LispPtr& aArguments)
{
    TESTARGS(2);
    // Get operator
    CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
    LispStringPtr orig = Argument(aArguments,1).Get()->String();
    CHK_ARG(orig != NULL, 1);
    aEnvironment.InFix().SetRightAssociative(SymbolName(aEnvironment,orig->String()));
    InternalTrue(aEnvironment,aResult);
}


void LispLeftPrecedence(LispEnvironment& aEnvironment, LispPtr& aResult,
                          LispPtr& aArguments)
{
    TESTARGS(3);
    // Get operator
    CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
    LispStringPtr orig = Argument(aArguments,1).Get()->String();
    CHK_ARG(orig != NULL, 1);

    LispPtr index;
    InternalEval(aEnvironment, index, Argument(aArguments,2));
    CHK_ARG(index.Get() != NULL, 2);
    CHK_ARG(index.Get()->String() != NULL, 2);
    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());

    aEnvironment.InFix().SetLeftPrecedence(SymbolName(aEnvironment,orig->String()),ind);
    InternalTrue(aEnvironment,aResult);
}


void LispRightPrecedence(LispEnvironment& aEnvironment, LispPtr& aResult,
                          LispPtr& aArguments)
{
    TESTARGS(3);
    // Get operator
    CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
    LispStringPtr orig = Argument(aArguments,1).Get()->String();
    CHK_ARG(orig != NULL, 1);

    LispPtr index;
    InternalEval(aEnvironment, index, Argument(aArguments,2));
    CHK_ARG(index.Get() != NULL, 2);
    CHK_ARG(index.Get()->String() != NULL, 2);
    LispInt ind = InternalAsciiToInt(index.Get()->String()->String());

    aEnvironment.InFix().SetRightPrecedence(SymbolName(aEnvironment,orig->String()),ind);
    InternalTrue(aEnvironment,aResult);
}
















static LispInFixOperator* OperatorInfo(LispEnvironment& aEnvironment,
                                       LispPtr& aArguments,
                                       LispOperators & aOperators)
{
    TESTARGS(2);
    // Get operator
    CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);

    //
    LispInFixOperator* op = aOperators.LookUp(
                                              SymbolName(aEnvironment,orig->String()));
    return op;
}


void LispIsInFix(LispEnvironment& aEnvironment, LispPtr& aResult,
               LispPtr& aArguments)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aArguments,
                                         aEnvironment.InFix());
    if (op != NULL)
        InternalTrue( aEnvironment, aResult);
    else
        InternalFalse(aEnvironment, aResult);
}

void LispIsBodied(LispEnvironment& aEnvironment, LispPtr& aResult,
               LispPtr& aArguments)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aArguments,
                                         aEnvironment.Bodied());
    if (op != NULL)
        InternalTrue( aEnvironment, aResult);
    else
        InternalFalse(aEnvironment, aResult);
}

void LispGetPrecedence(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aArguments,
                                         aEnvironment.InFix());
    if (op == NULL) {	// also need to check for a postfix or prefix operator
	    op = OperatorInfo(aEnvironment,
                          aArguments,
                          aEnvironment.PreFix());
        if (op == NULL) {
			op = OperatorInfo(aEnvironment,
                              aArguments,
                              aEnvironment.PostFix());
	        if (op == NULL) {	// or maybe it's a bodied function
				op = OperatorInfo(aEnvironment,
                              aArguments,
                              aEnvironment.Bodied());
    	 		CHK(op!=NULL, KLispErrIsNotInFix);
			}
		}
	}
    LispChar buf[30];
    InternalIntToAscii(buf, op->iPrecedence);
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(buf)));
}


void LispGetLeftPrecedence(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aArguments,
                                         aEnvironment.InFix());
    if (op == NULL) {	// infix and postfix operators have left precedence
	    op = OperatorInfo(aEnvironment,
                          aArguments,
                          aEnvironment.PostFix());
   	 	CHK(op!=NULL, KLispErrIsNotInFix);
	}

    LispChar buf[30];
    InternalIntToAscii(buf, op->iLeftPrecedence);
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(buf)));
}
void LispGetRightPrecedence(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispPtr& aArguments)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aArguments,
                                         aEnvironment.InFix());
    if (op == NULL) {   // bodied, infix and prefix operators have right precedence
        op = OperatorInfo(aEnvironment,
                          aArguments,
                          aEnvironment.PreFix());
        if (op == NULL) {   // or maybe it's a bodied function
            op = OperatorInfo(aEnvironment,
                          aArguments,
                          aEnvironment.Bodied());
            CHK(op!=NULL, KLispErrIsNotInFix);
        }
    }

    LispChar buf[30];
    InternalIntToAscii(buf, op->iRightPrecedence);
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(buf)));
}



void LispIsPreFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aArguments,
                                         aEnvironment.PreFix());
    if (op != NULL)
        InternalTrue( aEnvironment, aResult);
    else
        InternalFalse(aEnvironment, aResult);
}

void LispIsPostFix(LispEnvironment& aEnvironment, LispPtr& aResult,
                   LispPtr& aArguments)
{

    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aArguments,
                                         aEnvironment.PostFix());
    if (op != NULL)
        InternalTrue( aEnvironment, aResult);
    else
        InternalFalse(aEnvironment, aResult);
}

void LispGetPrecision(LispEnvironment& aEnvironment, LispPtr& aResult,
                      LispPtr& aArguments)
{
    TESTARGS(1);
    LispChar buf[30];
    InternalIntToAscii(buf, aEnvironment.Precision());
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(buf)));
}



void LispToString(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    TESTARGS(2);

    LispString oper;
    StringOutput newOutput(oper);

    LispLocalOutput localOutput(aEnvironment, &newOutput);

    // Evaluate the body
    InternalEval(aEnvironment, aResult, Argument(aArguments,1));

    //Return the result
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(oper.String())));
}

void LispSecure(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{
    TESTARGS(2);
    LispSecureFrame security(aEnvironment);
    InternalEval(aEnvironment, aResult, Argument(aArguments,1));
}


void LispFindFile(LispEnvironment& aEnvironment,LispPtr& aResult,
              LispPtr& aArguments)
{

    TESTARGS(2);

    CHK(aEnvironment.iSecure == 0, KLispErrSecurityBreach);
    
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get file name
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispChar filename[1024];//TODO FIXME
    InternalFindFile(oper.String(), aEnvironment.iInputDirectories,
                     filename);
    LispString res(filename,1);
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(res.String())));
}


void LispIsGeneric(LispEnvironment& aEnvironment,LispPtr& aResult,
                   LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    if (evaluated.Get()->Generic() != NULL)
        InternalTrue( aEnvironment, aResult);
    else
        InternalFalse(aEnvironment, aResult);
}

void LispGenericTypeName(LispEnvironment& aEnvironment,LispPtr& aResult,
                         LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    CHK_ARG(evaluated.Get()->Generic() != NULL,1);

    LispCharPtr name = evaluated.Get()->Generic()->TypeName();
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(name)));
}

void GenArrayCreate(LispEnvironment& aEnvironment,LispPtr& aResult,
                    LispPtr& aArguments)
{
    TESTARGS(3);

    LispPtr sizearg;
    InternalEval(aEnvironment, sizearg, Argument(aArguments,1));

    CHK_ARG(sizearg.Get() != NULL, 1);
    CHK_ARG(sizearg.Get()->String() != NULL, 1);

    LispInt size = InternalAsciiToInt(sizearg.Get()->String()->String());

    LispPtr initarg;
    InternalEval(aEnvironment, initarg, Argument(aArguments,2));
     
    ArrayClass *array = NEW ArrayClass(size,initarg.Get());
    aResult.Set(LispGenericClass::New(array));
}

void GenArraySize(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    GenericClass *gen = evaluated.Get()->Generic();
    CHK_ARG(gen != NULL,1);
    CHK_ARG(StrEqual(gen->TypeName(),"\"Array\""),1);
    LispInt size=((ArrayClass*)gen)->Size();
    LispChar s[20];
    InternalIntToAscii(s,size);
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(s)));
}

void GenArrayGet(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(3);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    GenericClass *gen = evaluated.Get()->Generic();
    CHK_ARG(gen != NULL,1);
    CHK_ARG(StrEqual(gen->TypeName(),"\"Array\""),1);

    LispPtr sizearg;
    InternalEval(aEnvironment, sizearg, Argument(aArguments,2));

    CHK_ARG(sizearg.Get() != NULL, 2);
    CHK_ARG(sizearg.Get()->String() != NULL, 2);

    LispInt size = InternalAsciiToInt(sizearg.Get()->String()->String());


    CHK_ARG(size>0 && size<=((ArrayClass*)gen)->Size(),2);
    LispObject* object = ((ArrayClass*)gen)->GetElement(size);

    aResult.Set(object->Copy(LispFalse));
}


void GenArraySet(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(4);

    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    GenericClass *gen = evaluated.Get()->Generic();
    CHK_ARG(gen != NULL,1);
    CHK_ARG(StrEqual(gen->TypeName(),"\"Array\""),1);

    LispPtr sizearg;
    InternalEval(aEnvironment, sizearg, Argument(aArguments,2));

    CHK_ARG(sizearg.Get() != NULL, 2);
    CHK_ARG(sizearg.Get()->String() != NULL, 2);

    LispInt size = InternalAsciiToInt(sizearg.Get()->String()->String());
    CHK_ARG(size>0 && size<=((ArrayClass*)gen)->Size(),2);

    LispPtr obj;
    InternalEval(aEnvironment, obj, Argument(aArguments,3));
    ((ArrayClass*)gen)->SetElement(size,obj.Get());
    InternalTrue( aEnvironment, aResult);
}

void LispCustomEval(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
  TESTARGS(5);
  if (aEnvironment.iDebugger) delete aEnvironment.iDebugger;
  aEnvironment.iDebugger = NEW DefaultDebugger(Argument(aArguments,1), Argument(aArguments,2),Argument(aArguments,3));
  LispLocalEvaluator local(aEnvironment,NEW TracedEvaluator);
  aEnvironment.iDebugger->Start();
  InternalEval(aEnvironment, aResult, Argument(aArguments,4));
  aEnvironment.iDebugger->Finish();
  delete aEnvironment.iDebugger;
  aEnvironment.iDebugger = NULL;
}

void LispCustomEvalExpression(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
  TESTARGS(1);
  if (aEnvironment.iDebugger == NULL)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  aResult.Set(aEnvironment.iDebugger->iTopExpr.Get()); 
}
void LispCustomEvalResult(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
  TESTARGS(1);
  if (aEnvironment.iDebugger == NULL)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  aResult.Set(aEnvironment.iDebugger->iTopResult.Get()); 
}



void LispCustomEvalLocals(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
  TESTARGS(1);
  aEnvironment.CurrentLocals(aResult);
}

void LispCustomEvalStop(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
  TESTARGS(1);
  if (aEnvironment.iDebugger == NULL)
  {
    RaiseError("Trying to get CustomEval results while not in custom evaluation");
  }
  aEnvironment.iDebugger->iStopped = LispTrue;

  InternalTrue(aEnvironment,aResult);
}

void LispTraceStack(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(2);
    LispLocalEvaluator local(aEnvironment,NEW TracedStackEvaluator);
    InternalEval(aEnvironment, aResult, Argument(aArguments,1));
}


void LispReadLisp(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
    LispParser parser(tok,
                      *aEnvironment.CurrentInput(),
                      aEnvironment);
    // Read expression
    parser.Parse(aResult);
}
void LispReadLispListed(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
    LispParser parser(tok,
                      *aEnvironment.CurrentInput(),
                      aEnvironment);
    parser.iListed = LispTrue;
    // Read expression
    parser.Parse(aResult);
}


void LispTraceRule(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(3);
    LispPtr *ptr = aArguments.Get()->Next().Get()->SubList();
    LispUserFunction* userfunc=NULL;
    if (ptr != NULL)
        userfunc = GetUserFunction(aEnvironment,ptr);
    LispLocalTrace trace(userfunc);
    InternalEval(aEnvironment, aResult, Argument(aArguments,2));
}

void LispType(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));
    LispPtr* subList = evaluated.Get()->SubList();
    LispObject* head = NULL;
    if (!subList)
    {
        goto EMPTY;
    }
    head = subList->Get();
    if (!head->String())
        goto EMPTY;
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUpStringify(head->String()->String())));
    return;
    
EMPTY:
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp("\"\"")));
    return;
}



void LispStringMid(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(4);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,3));
    CHK_ISSTRING(evaluated,3);
    LispStringPtr orig = evaluated.Get()->String();
    
    LispPtr index;
    InternalEval(aEnvironment, index, Argument(aArguments,1));
    CHK_ARG(index.Get() != NULL, 1);
    CHK_ARG(index.Get()->String() != NULL, 1);
    LispInt from = InternalAsciiToInt(index.Get()->String()->String());
    CHK_ARG(from>0,1);
    
    InternalEval(aEnvironment, index, Argument(aArguments,2));
    CHK_ARG(index.Get() != NULL, 2);
    CHK_ARG(index.Get()->String() != NULL, 2);
    LispInt count = InternalAsciiToInt(index.Get()->String()->String());

    
    LispString str;
    str.SetNrItems(0);
    str.Append('\"');
    LispInt i;
    CHK(from+count<orig->NrItems()-1, KLispErrInvalidArg);
    for (i=from;i<from+count;i++)
        str.Append((*orig)[i]);
    str.Append('\"');
    str.Append('\0');
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(str.String())));
}


void LispSetStringMid(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(4);
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,3));
    CHK_ISSTRING(evaluated,3);
    LispStringPtr orig = evaluated.Get()->String();
    LispPtr index;
    InternalEval(aEnvironment, index, Argument(aArguments,1));
    CHK_ARG(index.Get() != NULL, 1);
    CHK_ARG(index.Get()->String() != NULL, 1);
    LispInt from = InternalAsciiToInt(index.Get()->String()->String());

    CHK_ARG(from>0,1);
    
    LispPtr ev2;
    InternalEval(aEnvironment, ev2, Argument(aArguments,2));
    CHK_ISSTRING(ev2,2);
    LispStringPtr replace = ev2.Get()->String();

    LispString str(orig->String());
    LispInt i;
    LispInt count = replace->NrItems();
    CHK(from+count-3<orig->NrItems()-1, KLispErrInvalidArg);

    for (i=0;i<count-3;i++)
        str[i+from] = (*replace)[i+1];
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(str.String())));
}



void LispFindFunction(LispEnvironment& aEnvironment,LispPtr& aResult,
                      LispPtr& aArguments)
{
    TESTARGS(2);
    CHK(aEnvironment.iSecure == 0, KLispErrSecurityBreach);
    
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, Argument(aArguments,1));

    // Get file name
    CHK_ARG(evaluated.Get() != NULL, 1);
    LispStringPtr orig = evaluated.Get()->String();
    CHK_ARG(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper.String()));
    if (multiUserFunc != NULL)
    {
        LispDefFile* def = multiUserFunc->iFileToOpen;
        if (def != NULL)
        {
            aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp(def->iFileName()->String())));
            return;
        }
    }
    aResult.Set(LispAtom::New(aEnvironment,aEnvironment.HashTable().LookUp("\"\"")));
}




















void GenPatternCreate(LispEnvironment& aEnvironment,LispPtr& aResult,
                      LispPtr& aArguments)
{
    TESTARGS(3);
    LispPtr pattern;
    InternalEval(aEnvironment, pattern, Argument(aArguments,1));
    LispPtr postpredicate;
    InternalEval(aEnvironment, postpredicate, Argument(aArguments,2));

    LispIterator iter(pattern);
    CHK_ARG(iter() != NULL,1);
    CHK_ARG(iter()->SubList() != NULL,1);
    iter.GoSub();
    CHK_ARG(iter() != NULL,1);
    iter.GoNext();

    LispPtr *ptr = iter.Ptr();


    YacasPatternPredicateBase* matcher =
        NEW YacasPatternPredicateBase(aEnvironment, *ptr,postpredicate);
    PatternClass *p = NEW PatternClass(matcher);
    aResult.Set(LispGenericClass::New(p));
}
void GenPatternMatches(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(3);
    LispPtr pattern;
    InternalEval(aEnvironment, pattern, Argument(aArguments,1));
    GenericClass *gen = pattern.Get()->Generic();
    CHK_ARG(gen != NULL,1);
    CHK_ARG(StrEqual(gen->TypeName(),"\"Pattern\""),1);

    LispPtr list;
    InternalEval(aEnvironment, list, Argument(aArguments,2));

    PatternClass *patclass = (PatternClass*)gen;

    LispIterator iter(list);
    CHK_ARG(iter() != NULL,2);
    CHK_ARG(iter()->SubList() != NULL,2);
    iter.GoSub();
    CHK_ARG(iter() != NULL,2);
    iter.GoNext();

    LispPtr *ptr = iter.Ptr();
    CHK_ARG(ptr != NULL,2);
    LispBoolean matches = patclass->Matches(aEnvironment,*ptr);
    InternalBoolean(aEnvironment,aResult,matches);
}

void LispRuleBaseDefined(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(3);
    LispPtr name;
    InternalEval(aEnvironment, name, Argument(aArguments,1));
    LispStringPtr orig = name.Get()->String();
    CHK_ARG(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispPtr sizearg;
    InternalEval(aEnvironment, sizearg, Argument(aArguments,2));
    CHK_ARG(sizearg.Get() != NULL, 2);
    CHK_ARG(sizearg.Get()->String() != NULL, 2);

    LispInt arity = InternalAsciiToInt(sizearg.Get()->String()->String());

    LispUserFunction* userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper.String()),arity);
    InternalBoolean(aEnvironment,aResult,userFunc != NULL);
}

void LispDefLoadFunction(LispEnvironment& aEnvironment,LispPtr& aResult,
                         LispPtr& aArguments)
{
    TESTARGS(2);
    LispPtr name;
    InternalEval(aEnvironment, name, Argument(aArguments,1));
    LispStringPtr orig = name.Get()->String();
    CHK_ARG(orig != NULL, 1);
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
    InternalTrue(aEnvironment,aResult);
}


void LispRuleBaseArgList(LispEnvironment& aEnvironment,LispPtr& aResult, LispPtr& aArguments)
{
    TESTARGS(3);
    LispPtr name;
    InternalEval(aEnvironment, name, Argument(aArguments,1));
    LispStringPtr orig = name.Get()->String();
    CHK_ARG(orig != NULL, 1);
    LispString oper;
    InternalUnstringify(oper, orig);

    LispPtr sizearg;
    InternalEval(aEnvironment, sizearg, Argument(aArguments,2));
    CHK_ARG(sizearg.Get() != NULL, 2);
    CHK_ARG(sizearg.Get()->String() != NULL, 2);

    LispInt arity = InternalAsciiToInt(sizearg.Get()->String()->String());

    LispUserFunction* userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper.String()),arity);
    CHK(userFunc != NULL, KLispErrInvalidArg);

    LispPtr& list = userFunc->ArgList();
    LispPtr head;
    head.Set(LispAtom::New(aEnvironment,aEnvironment.iList));
    head.Get()->Next().Set(list.Get());
    aResult.Set(LispSubList::New(head.Get()));
}


static void InternalNewRulePattern(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispPtr& aArguments, LispBoolean aMacroMode)
{
    TESTARGS(6);

    LispInt arity;
    LispInt precedence;

    LispPtr ar;
    LispPtr pr;
    LispPtr predicate;
    LispPtr body;
    LispStringPtr orig=NULL;
    
    // Get operator
    if (aMacroMode)
    {
        LispPtr result;
        InternalEval(aEnvironment, result, Argument(aArguments,1));
        CHK_ARG(result.Get() != NULL, 1);
        orig = result.Get()->String();
        CHK_ARG(orig != NULL, 1);

        InternalEval(aEnvironment, ar, Argument(aArguments,2));
        InternalEval(aEnvironment, pr, Argument(aArguments,3));
        InternalEval(aEnvironment, predicate, Argument(aArguments,4));
        InternalEval(aEnvironment, body, Argument(aArguments,5));
    }
    else
    {
        CHK_ARG(Argument(aArguments,1).Get() != NULL, 1);
        orig = Argument(aArguments,1).Get()->String();
        CHK_ARG(orig != NULL, 1);
        ar.Set(Argument(aArguments,2).Get());
        pr.Set(Argument(aArguments,3).Get());
        predicate.Set(Argument(aArguments,4).Get());
        body.Set(Argument(aArguments,5).Get());
    }
    
    // The arity
    CHK_ARG(ar.Get() != NULL, 2);
    CHK_ARG(ar.Get()->String() != NULL, 2);
    arity = InternalAsciiToInt(ar.Get()->String()->String());

    // The precedence
    CHK_ARG(pr.Get() != NULL, 3);
    CHK_ARG(pr.Get()->String() != NULL, 3);
    precedence = InternalAsciiToInt(pr.Get()->String()->String());
    
    // Finally define the rule base
    aEnvironment.DefineRulePattern(SymbolName(aEnvironment,orig->String()),
                            arity,
                            precedence,
                            predicate,
                            body );

    // Return LispTrue
    InternalTrue(aEnvironment,aResult);
}

void LispNewRulePattern(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalNewRulePattern(aEnvironment, aResult,aArguments, LispFalse);
}

void LispMacroNewRulePattern(LispEnvironment& aEnvironment, LispPtr& aResult,
                  LispPtr& aArguments)
{
    InternalNewRulePattern(aEnvironment, aResult,aArguments, LispTrue);
}



