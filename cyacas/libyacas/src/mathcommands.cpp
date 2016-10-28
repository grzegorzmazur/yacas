
#include "yacas/lispenvironment.h"
#include "yacas/standard.h"
#include "yacas/lispeval.h"
#include "yacas/lispatom.h"
#include "yacas/lispparser.h"
#include "yacas/platfileio.h"
#include "yacas/stringio.h"
#include "yacas/lisperror.h"
#include "yacas/infixparser.h"
#include "yacas/lispuserfunc.h"
#include "yacas/mathuserfunc.h"
#include "yacas/platmath.h"
#include "yacas/numbers.h"
#include "yacas/arrayclass.h"
#include "yacas/associationclass.h"
#include "yacas/patternclass.h"
#include "yacas/substitute.h"
#include "yacas/errors.h"
#include "yacas/arggetter.h"
#include "yacas/string_utils.h"

#include <string>
#include <cstring>
#include <limits.h>
#include <stdlib.h>
#include <sstream>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack[aStackTop]
#define ARGUMENT(i) aEnvironment.iStack[aStackTop+i]


class Command {
public:
    Command(LispEnvironment& env);

    virtual void Evaluate();

protected:
    LispEnvironment& _env;
};

void LispLexCompare2(LispEnvironment& aEnvironment, int aStackTop,
                     bool (*lexfunc)(const char* f1, const char* f2, LispHashTable& aHashTable,int aPrecision),
                     bool (*numfunc)(BigNumber& n1, BigNumber& n2)
                    );


void LispQuote(LispEnvironment& aEnvironment,int aStackTop)
{
    RESULT = (ARGUMENT(1)->Copy());
}

void LispEval(LispEnvironment& aEnvironment,int aStackTop)
{
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
}

/// Execute the Yacas commands \c Set and \c MacroSet.
/// The argument \a aMacroMode determines whether the first argument
/// should be evaluated. The real work is done by
/// LispEnvironment::SetVariable() .
/// \sa LispSetVar(), LispMacroSetVar()
static void InternalSetVar(LispEnvironment& aEnvironment, int aStackTop, bool aMacroMode, bool aGlobalLazyVariable)
{
    const LispString* varstring=nullptr;
    if (aMacroMode)
    {
        LispPtr result;
        InternalEval(aEnvironment, result, ARGUMENT(1));
        varstring = result->String();
    }
    else
    {
        varstring = ARGUMENT(1)->String();
    }
    CheckArg(varstring, 1, aEnvironment, aStackTop);
    CheckArg(!IsNumber(varstring->c_str(), true), 1, aEnvironment, aStackTop);

    LispPtr result;
    InternalEval(aEnvironment, result, ARGUMENT(2));
    aEnvironment.SetVariable(varstring, result, aGlobalLazyVariable);
    InternalTrue(aEnvironment,RESULT);
}

/// Corresponds to the Yacas function \c Set.
/// This function simply calls InternalSetVar() .
void LispSetVar(LispEnvironment& aEnvironment, int aStackTop)
{
  InternalSetVar(aEnvironment, aStackTop, false,false);
}

/// Corresponds to the Yacas function \c MacroSet.
/// This function simply calls InternalSetVar() .
void LispMacroSetVar(LispEnvironment& aEnvironment, int aStackTop)
{
  InternalSetVar(aEnvironment, aStackTop, true,false);
}

void LispSetGlobalLazyVariable(LispEnvironment& aEnvironment, int aStackTop)
{
  InternalSetVar(aEnvironment, aStackTop, false,true);
}


void LispClearVar(LispEnvironment& aEnvironment,int aStackTop)
{
  LispPtr* subList = ARGUMENT(1)->SubList();
  if (subList)
  {
    LispIterator iter(*subList);
    for (int nr=1; (++iter).getObj(); nr++)
    {
      const LispString* str = iter.getObj()->String();
      CheckArg(str, nr, aEnvironment, aStackTop);
      aEnvironment.UnsetVariable(str);
    }
  }
  InternalTrue(aEnvironment,RESULT);
}


/* StrCompare returns f1-f2: if f1 < f2 it returns -1, if f1=f2 it
 returns 0, and it returns 1 if f1>f2
 */
// the aPrecision argument is ignored here
static bool LexLessThan(const char* f1, const char* f2, LispHashTable& aHashTable,int aPrecision)
{
    return (std::strcmp(f1, f2) < 0);
}

// the aPrecision argument is ignored here
static bool LexGreaterThan(const char* f1, const char* f2, LispHashTable& aHashTable,int aPrecision)
{
    return (std::strcmp(f1, f2) > 0);
}

static bool BigLessThan(BigNumber& n1, BigNumber& n2)
{
  return n1.LessThan(n2) && !n1.Equals(n2);
}
static bool BigGreaterThan(BigNumber& n1, BigNumber& n2)
{
  return !(n1.LessThan(n2) || n1.Equals(n2));
}


void LispStrictTotalOrder(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr e1(ARGUMENT(1));
    LispPtr e2(ARGUMENT(2));
    
    InternalBoolean(aEnvironment,RESULT, InternalStrictTotalOrder(aEnvironment, e1, e2));
}

void LispLessThan(LispEnvironment& aEnvironment, int aStackTop)
{
    LispLexCompare2(aEnvironment, aStackTop, LexLessThan, BigLessThan);
}

void LispGreaterThan(LispEnvironment& aEnvironment, int aStackTop)
{
    LispLexCompare2(aEnvironment, aStackTop, LexGreaterThan, BigGreaterThan);
}


void LispLexCompare2(LispEnvironment& aEnvironment, int aStackTop,
                     bool (*lexfunc)(const char* f1, const char* f2, LispHashTable& aHashTable,int aPrecision),
                     bool (*numfunc)(BigNumber& n1, BigNumber& n2)
                    )
{
    LispPtr result1(ARGUMENT(1));
    LispPtr result2(ARGUMENT(2));
    bool cmp;
    RefPtr<BigNumber> n1; n1 = result1->Number(aEnvironment.Precision());
    RefPtr<BigNumber> n2; n2 = result2->Number(aEnvironment.Precision());
    if (!!n1 && !!n2)
    {
      cmp =numfunc(*n1,*n2);
    }
    else
    {
      const LispString* str1 = result1->String();
      const LispString* str2 = result2->String();
      CheckArg(str1, 1, aEnvironment, aStackTop);
      CheckArg(str2, 2, aEnvironment, aStackTop);
       // the precision argument is ignored in "lex" functions
      cmp = lexfunc(str1->c_str(),str2->c_str(),
                            aEnvironment.HashTable(),
                            aEnvironment.Precision());
    }

    InternalBoolean(aEnvironment,RESULT, cmp);
}



void LispFullForm(LispEnvironment& aEnvironment, int aStackTop)
{
    RESULT = (ARGUMENT(1));
    LispPrinter printer;
    printer.Print(RESULT, aEnvironment.CurrentOutput(), aEnvironment);
    aEnvironment.CurrentOutput().put('\n');
}


void LispHead(LispEnvironment& aEnvironment, int aStackTop)
{
  InternalNth(RESULT, ARGUMENT(1),1);
}


void LispNth(LispEnvironment& aEnvironment, int aStackTop)
{
    const LispString* str = ARGUMENT(2)->String();
    CheckArg(str, 2, aEnvironment, aStackTop);
    CheckArg(IsNumber(str->c_str(), false), 2, aEnvironment, aStackTop);
    int index = InternalAsciiToInt(*str);
    InternalNth(RESULT, ARGUMENT(1), index);
}


void LispTail(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr first;
    InternalTail(first, ARGUMENT(1));
    InternalTail(RESULT, first);
    LispPtr head(aEnvironment.iList->Copy());
    head->Nixed() = ((*RESULT->SubList()));
    (*RESULT->SubList()) = (head);
}

void LispUnList(LispEnvironment& aEnvironment, int aStackTop)
{
  CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
  CheckArg(ARGUMENT(1)->SubList(), 1, aEnvironment, aStackTop);
  LispObject* subList = (*ARGUMENT(1)->SubList());
  CheckArg(subList, 1, aEnvironment, aStackTop);
  CheckArg(subList->String() == aEnvironment.iList->String(), 1, aEnvironment, aStackTop);
  InternalTail(RESULT, ARGUMENT(1));
}

void LispListify(LispEnvironment& aEnvironment, int aStackTop)
{
  CheckArg(ARGUMENT(1)->SubList(), 1, aEnvironment, aStackTop);
  LispPtr head(aEnvironment.iList->Copy());
  head->Nixed() = ((*ARGUMENT(1)->SubList()));
  RESULT = (LispSubList::New(head));
}




void LispDestructiveReverse(LispEnvironment& aEnvironment, int aStackTop)
{
    CheckArgIsList(1, aEnvironment, aStackTop);

  LispPtr reversed(aEnvironment.iList->Copy());
  InternalReverseList(reversed->Nixed(), (*ARGUMENT(1)->SubList())->Nixed());
  RESULT = (LispSubList::New(reversed));
}


void LispLength(LispEnvironment& aEnvironment, int aStackTop)
{
    std::size_t size = 0;

    if (LispPtr* subList = ARGUMENT(1)->SubList()) {
        size = InternalListLength((*subList)->Nixed());
    } else if (InternalIsString(ARGUMENT(1)->String())) {
        size = ARGUMENT(1)->String()->size()-2;
    } else if (ArrayClass* arr = dynamic_cast<ArrayClass*>(ARGUMENT(1)->Generic())) {
        size = arr->Size();
    } else if (AssociationClass* assoc = dynamic_cast<AssociationClass*>(ARGUMENT(1)->Generic())) {
        size = assoc->Size();
    } else
        CheckArg(false, 1, aEnvironment, aStackTop);

    RESULT = LispAtom::New(aEnvironment, std::to_string(size));
}

void LispList(LispEnvironment& aEnvironment, int aStackTop)
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


void LispConcatenate(LispEnvironment& aEnvironment, int aStackTop)
{
  LispPtr all(aEnvironment.iList->Copy());
  LispIterator tail(all);
  ++tail;
  LispIterator iter(*ARGUMENT(1)->SubList());
  for (int arg = 1; (++iter).getObj(); arg++)
  {
    CheckArgIsList(*iter, arg, aEnvironment, aStackTop);
    InternalFlatCopy(*tail,(*(*iter)->SubList())->Nixed());  // TODO: woof -- prefer below
    //InternalFlatCopy(*tail,iter.getObj()->Nixed());
    while (tail.getObj()) ++tail;
  }
  RESULT = (LispSubList::New(all));
}

void LispConcatenateStrings(LispEnvironment& aEnvironment, int aStackTop)
{
    std::string s;
    s.push_back('\"');

    int arg = 1;
    for (LispIterator iter(*ARGUMENT(1)->SubList()); (++iter).getObj();) {
        CheckArgIsString(*iter, arg++, aEnvironment, aStackTop);
        const std::string& p = *iter.getObj()->String();
        s.append(p.substr(1, p.size() - 2));
    }
    s.push_back('\"');

    RESULT = LispAtom::New(aEnvironment, s);
}

static void InternalDelete(LispEnvironment& aEnvironment, int aStackTop, int aDestructive)
{
    LispPtr evaluated(ARGUMENT(1));
    CheckArgIsList(1, aEnvironment, aStackTop);

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
    CheckArg(index, 2, aEnvironment, aStackTop);
    CheckArg(index->String(), 2, aEnvironment, aStackTop);
    int ind = InternalAsciiToInt(*index->String());
    CheckArg(ind > 0, 2, aEnvironment, aStackTop);

    LispIterator iter(copied);
    while (--ind >= 0)
        ++iter;
    if (!iter.getObj()) {
        ShowStack(aEnvironment);
        throw LispErrListNotLongEnough();
    }
    LispIterator temp = iter++;
    (*temp) = (*iter);
    RESULT = (LispSubList::New(copied));
}

void LispDelete(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalDelete(aEnvironment, aStackTop,false);
}

void LispDestructiveDelete(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalDelete(aEnvironment, aStackTop,true);
}

void LispFlatCopy(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr copied;

    if (ARGUMENT(1)->SubList() == nullptr)
        CheckArgIsList(1, aEnvironment, aStackTop);

    InternalFlatCopy(copied,*ARGUMENT(1)->SubList());
    RESULT = (LispSubList::New(copied));
}

static void InternalInsert(LispEnvironment& aEnvironment, int aStackTop, int aDestructive)
{
    CheckArgIsList(1, aEnvironment, aStackTop);

    LispPtr evaluated(ARGUMENT(1));

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
    CheckArg(index, 2, aEnvironment, aStackTop);
    CheckArg(index->String(), 2, aEnvironment, aStackTop);
    int ind = InternalAsciiToInt(*index->String());
    CheckArg(ind > 0, 2, aEnvironment, aStackTop);

    LispIterator iter(copied);
    while (--ind>=0) ++iter;
    LispPtr toInsert(ARGUMENT(3));
    toInsert->Nixed() = (iter.getObj());
    (*iter) = (toInsert);
    RESULT = (LispSubList::New(copied));
}

void LispInsert(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalInsert(aEnvironment, aStackTop,false);
}

void LispDestructiveInsert(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalInsert(aEnvironment, aStackTop,true);
}

static void InternalReplace(LispEnvironment& aEnvironment, int aStackTop, int aDestructive)
{
    LispPtr evaluated(ARGUMENT(1));
//    CHK_ISLIST_CORE(evaluated,1);
    // Ok, so lets not check if it is a list, but it needs to be at least a 'function'
    CheckArg(evaluated->SubList(), 1, aEnvironment, aStackTop);

    LispPtr index(ARGUMENT(2));
    CheckArg(index, 2, aEnvironment, aStackTop);
    CheckArg(index->String(), 2, aEnvironment, aStackTop);
    int ind = InternalAsciiToInt(*index->String());

    LispPtr copied;
    if (aDestructive)
    {
        copied = ((*evaluated->SubList()));
    }
    else
    {
        InternalFlatCopy(copied,*evaluated->SubList());
    }
    CheckArg(ind > 0, 2, aEnvironment, aStackTop);

    LispIterator iter(copied);
    while (--ind>=0) ++iter;
    LispPtr toInsert(ARGUMENT(3));
    CheckArg(iter.getObj(), 2, aEnvironment, aStackTop);

    LispIterator temp = iter++;
    toInsert->Nixed() = (*iter);
    (*temp) = (toInsert);
    RESULT = (LispSubList::New(copied));
}

void LispReplace(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalReplace(aEnvironment, aStackTop,false);
}

void LispDestructiveReplace(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalReplace(aEnvironment, aStackTop,true);
}

void LispNot(LispEnvironment& aEnvironment, int aStackTop)
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

void LispLazyAnd(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr nogos;
    int nrnogos = 0;
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

void LispLazyOr(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr nogos;
    int nrnogos = 0;

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

void LispEquals(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr evaluated1(ARGUMENT(1));
    LispPtr evaluated2(ARGUMENT(2));

    InternalBoolean(aEnvironment,RESULT,
                    InternalEquals(aEnvironment, evaluated1, evaluated2));
}

void LispWrite(LispEnvironment& aEnvironment, int aStackTop)
{
  LispPtr* subList = ARGUMENT(1)->SubList();
  if (subList)
  {
    LispIterator iter(*subList);
    while ((++iter).getObj())
    {
      aEnvironment.CurrentPrinter().Print(*iter,aEnvironment.CurrentOutput(),aEnvironment);
    }
  }
  InternalTrue(aEnvironment,RESULT);
}

void LispWriteString(LispEnvironment& aEnvironment, int aStackTop)
{
  CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
  const LispString* str = ARGUMENT(1)->String();
  CheckArg(str, 1, aEnvironment, aStackTop);
  CheckArg((*str)[0] == '\"', 1, aEnvironment, aStackTop);
  CheckArg((*str)[str->size()-1] == '\"', 1, aEnvironment, aStackTop);

  const std::size_t nr = str->size()-1;
  //((*str)[i] != '\"')
  for (std::size_t i = 1; i < nr; ++i)
    aEnvironment.CurrentOutput().put((*str)[i]);

  // pass last printed character to the current printer
  aEnvironment.CurrentPrinter().RememberLastChar((*str)[nr-1]);  // hacky hacky
  InternalTrue(aEnvironment,RESULT);
}

void LispProgBody(LispEnvironment& aEnvironment, int aStackTop)
{
  // Allow accessing previous locals.
  LispLocalFrame frame(aEnvironment,false);

  InternalTrue(aEnvironment,RESULT);

  // Evaluate args one by one.

  LispIterator iter(*ARGUMENT(1)->SubList());
  while ((++iter).getObj())
  {
    InternalEval(aEnvironment, RESULT, *iter);
  }
}

void LispNewLocal(LispEnvironment& aEnvironment, int aStackTop)
{
  LispPtr* subList = ARGUMENT(1)->SubList();
  if (subList)
  {
    LispIterator iter(*subList);
    for (int nr = 1; (++iter).getObj(); nr++)
    {
      const LispString* variable = iter.getObj()->String();
      CheckArg(variable, nr, aEnvironment, aStackTop);
// printf("Variable %s\n",variable->String());
      aEnvironment.NewLocal(variable,nullptr);
    }
  }
  InternalTrue(aEnvironment,RESULT);
}

void LispWhile(LispEnvironment& aEnvironment, int aStackTop)
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
    CheckArg(IsFalse(aEnvironment, predicate), 1, aEnvironment, aStackTop);
    InternalTrue(aEnvironment,RESULT);
}

static void MultiFix(LispEnvironment& aEnvironment, int aStackTop, LispOperators& aOps)
{

    // Get operator
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    LispPtr precedence;
    InternalEval(aEnvironment, precedence, ARGUMENT(2));
    CheckArg(precedence->String(), 2, aEnvironment, aStackTop);
    int prec = InternalAsciiToInt(*precedence->String());
    CheckArg(prec <= KMaxPrecedence, 2, aEnvironment, aStackTop);
    aOps[SymbolName(aEnvironment,*orig)] = LispInFixOperator(prec);
    InternalTrue(aEnvironment,RESULT);
}

void LispInFix(LispEnvironment& aEnvironment, int aStackTop)
{
    MultiFix(aEnvironment, aStackTop, aEnvironment.InFix());
}

static void SingleFix(int aPrecedence, LispEnvironment& aEnvironment, int aStackTop, LispOperators& aOps)
{
    // Get operator
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    aOps[SymbolName(aEnvironment,*orig)] = LispInFixOperator(aPrecedence);
    InternalTrue(aEnvironment,RESULT);
}

void LispPreFix(LispEnvironment& aEnvironment, int aStackTop)
{
/*
    int nrArguments = InternalListLength(ARGUMENT(0));
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

void LispPostFix(LispEnvironment& aEnvironment, int aStackTop)
{
    const std::size_t nrArguments = InternalListLength(ARGUMENT(0));
    if (nrArguments == 2)
        SingleFix(0, aEnvironment, aStackTop, aEnvironment.PostFix());
    else
        MultiFix(aEnvironment, aStackTop, aEnvironment.PostFix());
}

void LispBodied(LispEnvironment& aEnvironment, int aStackTop)
{
    MultiFix(aEnvironment, aStackTop, aEnvironment.Bodied());
}

void LispAtomize(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    // Get operator
    CheckArg(evaluated, 1, aEnvironment, aStackTop);
    const LispString* orig = evaluated->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    RESULT = LispAtom::New(aEnvironment, orig->substr(1, orig->length() - 2));
}

void LispStringify(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    // Get operator
    CheckArg(evaluated, 1, aEnvironment, aStackTop);
    const LispString* orig = evaluated->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    RESULT = LispAtom::New(aEnvironment, stringify(*orig));
}

void LispLoad(LispEnvironment& aEnvironment, int aStackTop)
{
    CheckSecure(aEnvironment, aStackTop);

    LispPtr evaluated(ARGUMENT(1));

    // Get file name
    CheckArg(evaluated, 1, aEnvironment, aStackTop);
    const LispString* orig = evaluated->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    InternalLoad(aEnvironment, *orig);
    InternalTrue(aEnvironment, RESULT);
}

void LispTmpFile(LispEnvironment& aEnvironment, int aStackTop)
{
    CheckSecure(aEnvironment, aStackTop);

#ifndef _WIN32
    char fn[] = "/tmp/yacas-XXXXXX";

    int fd = mkstemp(fn);

    // FIXME: not very clear
    if (fd < 0) {
        ShowStack(aEnvironment);
        throw LispErrFileNotFound();
    }
    
    close(fd);
    RESULT = LispAtom::New(aEnvironment, stringify(fn));

#else
    char tmp_path[MAX_PATH];
    char tmp_fn[MAX_PATH];

    GetTempPath(MAX_PATH, tmp_path);
    GetTempFileName(tmp_path, "yacas", 0, tmp_fn);

    RESULT = LispAtom::New(aEnvironment, stringify(tmp_fn));
#endif
}

void LispProtect(LispEnvironment& env, int top)
{
    LispPtr p(env.iStack[top + 1]);

    CheckArg(p, 1, env, top);
    const LispString* s = p->String();
    CheckArg(s, 1, env, top);

    env.Protect(s);

    InternalTrue(env, env.iStack[top]);
}

void LispUnProtect(LispEnvironment& env, int top)
{
    LispPtr p(env.iStack[top + 1]);

    CheckArg(p, 1, env, top);
    const LispString* s = p->String();
    CheckArg(s, 1, env, top);

    env.UnProtect(s);

    InternalTrue(env, env.iStack[top]);
}

void LispIsProtected(LispEnvironment& env, int top)
{
    LispPtr p(env.iStack[top + 1]);

    CheckArg(p, 1, env, top);
    const LispString* s = p->String();
    CheckArg(s, 1, env, top);

    env.iStack[top] = env.Protected(s) ? env.iTrue->Copy() : env.iFalse->Copy();
}

/// Implements the Yacas functions \c RuleBase and \c MacroRuleBase .
/// The real work is done by LispEnvironment::DeclareRuleBase().
static void InternalRuleBase(LispEnvironment& aEnvironment, int aStackTop,
                             int aListed)
{
    // Get operator

    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    LispPtr args(ARGUMENT(2));

    // The arguments
    CheckArgIsList(2, aEnvironment, aStackTop);

    // Finally define the rule base
    aEnvironment.DeclareRuleBase(SymbolName(aEnvironment,*orig),
                                 (*args->SubList())->Nixed(),aListed);

    // Return true
    InternalTrue(aEnvironment,RESULT);
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
    // Get operator
    //LispPtr body;

    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    // The arguments
    LispPtr args(ARGUMENT(2));
    CheckArgIsList(2, aEnvironment, aStackTop);

    // Finally define the rule base
    aEnvironment.DeclareMacroRuleBase(SymbolName(aEnvironment,*orig),
                                 (*args->SubList())->Nixed(),aListed);

    // Return true
    InternalTrue(aEnvironment,RESULT);
}

void LispDefMacroRuleBaseListed(LispEnvironment& aEnvironment, int aStackTop)
{
  InternalDefMacroRuleBase(aEnvironment, aStackTop, true);
}

void LispDefMacroRuleBase(LispEnvironment& aEnvironment, int aStackTop)
{
  InternalDefMacroRuleBase(aEnvironment, aStackTop, false);
}

void LispRuleBaseListed(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalRuleBase(aEnvironment, aStackTop, true);
}

void LispMacroRuleBaseListed(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalRuleBase(aEnvironment, aStackTop, true);
}

void LispHoldArg(LispEnvironment& aEnvironment, int aStackTop)
{
    // Get operator
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    // The arguments
    const LispString* tohold = ARGUMENT(2)->String();
    CheckArg(tohold, 2, aEnvironment, aStackTop);
    aEnvironment.HoldArgument(SymbolName(aEnvironment,*orig), tohold);
    // Return true
    InternalTrue(aEnvironment,RESULT);
}

static void InternalNewRule(LispEnvironment& aEnvironment, int aStackTop)
{
    int arity;
    int precedence;

    LispPtr ar;
    LispPtr pr;
    LispPtr predicate;
    LispPtr body;

    // Get operator
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    ar = (ARGUMENT(2));
    pr = (ARGUMENT(3));
    predicate = (ARGUMENT(4));
    body = (ARGUMENT(5));

    // The arity
    CheckArg(ar, 2, aEnvironment, aStackTop);
    CheckArg(ar->String(), 2, aEnvironment, aStackTop);
    arity = InternalAsciiToInt(*ar->String());

    // The precedence
    CheckArg(pr, 3, aEnvironment, aStackTop);
    CheckArg(pr->String(), 3, aEnvironment, aStackTop);
    precedence = InternalAsciiToInt(*pr->String());

    // Finally define the rule base
    aEnvironment.DefineRule(SymbolName(aEnvironment,*orig),
                            arity,
                            precedence,
                            predicate,
                            body );

    // Return true
    InternalTrue(aEnvironment,RESULT);
}

void LispNewRule(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalNewRule(aEnvironment, aStackTop);
}

void LispMacroNewRule(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalNewRule(aEnvironment, aStackTop);
}

void LispUnFence(LispEnvironment& aEnvironment, int aStackTop)
{
    // Get operator
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    // The arity
    CheckArg(ARGUMENT(2), 2, aEnvironment, aStackTop);
    CheckArg(ARGUMENT(2)->String(), 2, aEnvironment, aStackTop);
    int arity = InternalAsciiToInt(*ARGUMENT(2)->String());

    aEnvironment.UnFenceRule(SymbolName(aEnvironment,*orig),
                            arity);

    // Return true
    InternalTrue(aEnvironment,RESULT);
}

void LispIsFunction(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr result(ARGUMENT(1));
    InternalBoolean(aEnvironment,RESULT,
                    result->SubList()!=nullptr);
}

void LispIsAtom(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr result(ARGUMENT(1));
    InternalBoolean(aEnvironment,RESULT,
                    result->String()!=nullptr);
}

void LispIsNumber(LispEnvironment& aEnvironment,int aStackTop)
{
  LispPtr result(ARGUMENT(1));
  InternalBoolean(aEnvironment, RESULT, result->Number(aEnvironment.Precision()) != nullptr);
}

void LispIsInteger(LispEnvironment& aEnvironment,int aStackTop)
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

void LispIsList(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr result(ARGUMENT(1));
    InternalBoolean(aEnvironment,RESULT,InternalIsList(aEnvironment, result));
}

void LispIsString(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr result(ARGUMENT(1));
    InternalBoolean(aEnvironment,RESULT,
                    InternalIsString(result->String()));
}

void LispIsBound(LispEnvironment& aEnvironment,int aStackTop)
{
    const LispString* str = ARGUMENT(1)->String();
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

void LispIf(LispEnvironment& aEnvironment, int aStackTop)
{
    int nrArguments = InternalListLength(ARGUMENT(0));
    if (nrArguments != 3 && nrArguments != 4) {
        ShowStack(aEnvironment);
        throw LispErrWrongNumberOfArgs();
    }

    LispPtr predicate;
    InternalEval(aEnvironment, predicate, ARGUMENT(1));

    if (IsTrue(aEnvironment,predicate))
    {
        InternalEval(aEnvironment, RESULT, Argument(ARGUMENT(0),2));
    }
    else
    {
        CheckArg(IsFalse(aEnvironment, predicate), 1, aEnvironment, aStackTop);
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

void LispRetract(LispEnvironment& aEnvironment, int aStackTop)
{
    // Get operator
    LispPtr evaluated(ARGUMENT(1));

    CheckArg(evaluated, 1, aEnvironment, aStackTop);
    const LispString* orig = evaluated->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    const LispString* oper = SymbolName(aEnvironment,*orig);

    LispPtr arity(ARGUMENT(2));
    CheckArg(arity->String(), 2, aEnvironment, aStackTop);
    int ar = InternalAsciiToInt(*arity->String());
    aEnvironment.Retract(oper, ar);
    InternalTrue(aEnvironment,RESULT);
}

void YacasBuiltinPrecisionSet(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr index(ARGUMENT(1));
    CheckArg(index, 1, aEnvironment, aStackTop);
    CheckArg(index->String(), 1, aEnvironment, aStackTop);

    int ind = InternalAsciiToInt(*index->String());
    CheckArg(ind > 0, 1, aEnvironment, aStackTop);
    aEnvironment.SetPrecision(ind);
    InternalTrue(aEnvironment,RESULT);
}

void LispDefaultDirectory(LispEnvironment& aEnvironment, int aStackTop)
{
    // Get file name
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    aEnvironment.iInputDirectories.push_back(InternalUnstringify(*orig));
    InternalTrue(aEnvironment,RESULT);
}

void LispFromFile(LispEnvironment& aEnvironment, int aStackTop)
{
  CheckSecure(aEnvironment, aStackTop);

  LispPtr evaluated;
  InternalEval(aEnvironment, evaluated, ARGUMENT(1));

  // Get file name
  CheckArg(evaluated, 1, aEnvironment, aStackTop);
  const LispString* orig = evaluated->String();
  CheckArg(orig, 1, aEnvironment, aStackTop);

  const std::string fname = orig->substr(1, orig->length() - 2);

  InputStatus oldstatus = aEnvironment.iInputStatus;
  aEnvironment.iInputStatus.SetTo(fname);

  // Open file
  LispLocalFile localFP(aEnvironment, fname, true,
                        aEnvironment.iInputDirectories);
  if (!localFP.stream.is_open()) {
    ShowStack(aEnvironment);
    throw LispErrFileNotFound();
  }
  StdFileInput newInput(localFP,aEnvironment.iInputStatus);
  LispLocalInput localInput(aEnvironment, &newInput);

  // Evaluate the body
  InternalEval(aEnvironment, RESULT, ARGUMENT(2));

  aEnvironment.iInputStatus.RestoreFrom(oldstatus);
  //Return the result
}

void LispFromString(LispEnvironment& aEnvironment, int aStackTop)
{
  LispPtr evaluated;
  InternalEval(aEnvironment, evaluated, ARGUMENT(1));

  // Get file name
  CheckArg(evaluated, 1, aEnvironment, aStackTop);
  const LispString* orig = evaluated->String();
  CheckArg(orig, 1, aEnvironment, aStackTop);
  const std::string oper = InternalUnstringify(*orig);

  InputStatus oldstatus = aEnvironment.iInputStatus;
  aEnvironment.iInputStatus.SetTo("String");
  StringInput newInput(oper,aEnvironment.iInputStatus);
  LispLocalInput localInput(aEnvironment, &newInput);

  // Evaluate the body
  InternalEval(aEnvironment, RESULT, ARGUMENT(2));
  aEnvironment.iInputStatus.RestoreFrom(oldstatus);

  //Return the result
}

void LispRead(LispEnvironment& aEnvironment, int aStackTop)
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

void LispReadToken(LispEnvironment& aEnvironment, int aStackTop)
{
  LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
  const LispString* result =
    tok.NextToken(*aEnvironment.CurrentInput(), aEnvironment.HashTable());

  if (result->empty())
  {
    RESULT = aEnvironment.iEndOfFile->Copy();
    return;
  }
  RESULT = LispAtom::New(aEnvironment, *result);
}

void LispToFile(LispEnvironment& aEnvironment, int aStackTop)
{
  CheckSecure(aEnvironment, aStackTop);

  LispPtr evaluated;
  InternalEval(aEnvironment, evaluated, ARGUMENT(1));

  // Get file name
  CheckArg(evaluated, 1, aEnvironment, aStackTop);
  const LispString* orig = evaluated->String();
  CheckArg(orig, 1, aEnvironment, aStackTop);
  const std::string oper = InternalUnstringify(*orig);

  // Open file for writing
  LispLocalFile localFP(aEnvironment, oper, false, aEnvironment.iInputDirectories);
  if (!localFP.stream.is_open()) {
      ShowStack(aEnvironment);
      throw LispErrFileNotFound();
  }
  LispLocalOutput localOutput(aEnvironment, localFP.stream);

  // Evaluate the body
  InternalEval(aEnvironment, RESULT, ARGUMENT(2));

  //Return the result
}

void LispCheck(LispEnvironment& aEnvironment,int aStackTop)
{
  LispPtr pred;
  InternalEval(aEnvironment, pred, ARGUMENT(1));
  if (!IsTrue(aEnvironment,pred))
  {
    LispPtr evaluated;
    InternalEval(aEnvironment, evaluated, ARGUMENT(2));
    CheckArgIsString(evaluated, 2, aEnvironment, aStackTop);
    ShowStack(aEnvironment);
    throw LispErrUser(*evaluated->String());
  }
  RESULT = pred;
}

void LispTrapError(LispEnvironment& aEnvironment,int aStackTop)
{
    try {
        InternalEval(aEnvironment, RESULT, ARGUMENT(1));
    } catch (const LispError& error) {
        HandleError(error, aEnvironment, aEnvironment.iErrorOutput);
    }

  if (!aEnvironment.iErrorOutput.str().empty())
  {
    InternalEval(aEnvironment, RESULT, ARGUMENT(2));
    aEnvironment.iErrorOutput.clear();
    aEnvironment.iErrorOutput.str("");
  }
}

void LispGetCoreError(LispEnvironment& aEnvironment,int aStackTop)
{
  RESULT = LispAtom::New(aEnvironment, stringify(aEnvironment.iErrorOutput.str()));
}

void LispSystemCall(LispEnvironment& aEnvironment,int aStackTop)
{
  CheckSecure(aEnvironment, aStackTop);

  LispPtr result(ARGUMENT(1));
  CheckArgIsString(1, aEnvironment, aStackTop);

  const std::string command = InternalUnstringify(*result->String());

// we would like to pass the exit code back to Yacas. Right now, let's pass True/False according to whether the exit code is 0 or not.
  InternalBoolean(aEnvironment, RESULT, system(command.c_str()) == 0);
}

void LispSystemName(LispEnvironment& aEnvironment, int aStackTop)
{
    const char* s = "Unknown";

#if defined(_WIN32)
    s = "Windows";
#elif defined(__APPLE__)
    s = "MacOSX";
#elif defined (__linux__)
    s = "Linux";
#endif

    RESULT = LispAtom::New(aEnvironment, stringify(s));
}

void LispMaxEvalDepth(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr index(ARGUMENT(1));
    CheckArg(index, 1, aEnvironment, aStackTop);
    CheckArg(index->String(), 1, aEnvironment, aStackTop);

    int ind = InternalAsciiToInt(*index->String());
    aEnvironment.iMaxEvalDepth = ind;
    InternalTrue(aEnvironment,RESULT);
}

void LispDefLoad(LispEnvironment& aEnvironment, int aStackTop)
{
    CheckSecure(aEnvironment, aStackTop);

    LispPtr evaluated(ARGUMENT(1));

    // Get file name
    CheckArg(evaluated, 1, aEnvironment, aStackTop);
    const LispString* orig = evaluated->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    LoadDefFile(aEnvironment, *orig);
    InternalTrue(aEnvironment, RESULT);
}

void LispUse(LispEnvironment& aEnvironment, int aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    // Get file name
    CheckArg(evaluated, 1, aEnvironment, aStackTop);
    const LispString* orig = evaluated->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    InternalUse(aEnvironment, *orig);
    InternalTrue(aEnvironment, RESULT);
}

void LispRightAssociative(LispEnvironment& aEnvironment, int aStackTop)
{
    // Get operator
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    LispOperators::iterator opi = aEnvironment.InFix().find(SymbolName(aEnvironment,*orig));
    if (opi == aEnvironment.InFix().end())
        throw LispErrNotAnInFixOperator();
    opi->second.SetRightAssociative();
    
    InternalTrue(aEnvironment,RESULT);
}

void LispLeftPrecedence(LispEnvironment& aEnvironment, int aStackTop)
{
    // Get operator
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    LispPtr index;
    InternalEval(aEnvironment, index, ARGUMENT(2));
    CheckArg(index, 2, aEnvironment, aStackTop);
    CheckArg(index->String(), 2, aEnvironment, aStackTop);
    int ind = InternalAsciiToInt(*index->String());

    LispOperators::iterator opi = aEnvironment.InFix().find(SymbolName(aEnvironment,*orig));
    if (opi == aEnvironment.InFix().end())
        throw LispErrNotAnInFixOperator();
    opi->second.SetLeftPrecedence(ind);

    InternalTrue(aEnvironment,RESULT);
}

void LispRightPrecedence(LispEnvironment& aEnvironment, int aStackTop)
{
    // Get operator
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    LispPtr index;
    InternalEval(aEnvironment, index, ARGUMENT(2));
    CheckArg(index, 2, aEnvironment, aStackTop);
    CheckArg(index->String(), 2, aEnvironment, aStackTop);
    int ind = InternalAsciiToInt(*index->String());

    LispOperators::iterator opi = aEnvironment.InFix().find(SymbolName(aEnvironment,*orig));
    if (opi == aEnvironment.InFix().end())
        throw LispErrNotAnInFixOperator();
    opi->second.SetRightPrecedence(ind);

    InternalTrue(aEnvironment,RESULT);
}

static LispInFixOperator* OperatorInfo(LispEnvironment& aEnvironment,int aStackTop, LispOperators& aOperators)
{
    // Get operator
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);

    LispPtr evaluated(ARGUMENT(1));

    const LispString* orig = evaluated->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);

    const LispOperators::iterator opi = aOperators.find(SymbolName(aEnvironment,*orig));
    if (opi != aOperators.end())
        return &opi->second;
    return nullptr;
}

void LispIsInFix(LispEnvironment& aEnvironment, int aStackTop)
{
  LispInFixOperator* op = OperatorInfo(aEnvironment,
                                       aStackTop,
                                       aEnvironment.InFix());
  InternalBoolean(aEnvironment, RESULT, op != nullptr);
}

void LispIsBodied(LispEnvironment& aEnvironment, int aStackTop)
{
  LispInFixOperator* op = OperatorInfo(aEnvironment,
                                       aStackTop,
                                       aEnvironment.Bodied());
  InternalBoolean(aEnvironment, RESULT, op != nullptr);
}

void LispGetPrecedence(LispEnvironment& aEnvironment, int aStackTop)
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
        if (!op) {
            ShowStack(aEnvironment);
            throw LispErrIsNotInFix();
        }
      }
    }
  }
  RESULT = LispAtom::New(aEnvironment, std::to_string(op->iPrecedence));
}

void LispGetLeftPrecedence(LispEnvironment& aEnvironment, int aStackTop)
{
    LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.InFix());
    if (!op) {  // infix and postfix operators have left precedence
      op = OperatorInfo(aEnvironment,
                          aStackTop,
                          aEnvironment.PostFix());
      if (!op) {
          ShowStack(aEnvironment);
          throw LispErrIsNotInFix();
      }
  }

    RESULT = LispAtom::New(aEnvironment,std::to_string(op->iLeftPrecedence));
}

void LispGetRightPrecedence(LispEnvironment& aEnvironment, int aStackTop)
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
            if (!op) {
                ShowStack(aEnvironment);
                throw LispErrIsNotInFix();
            }
        }
    }

    RESULT = LispAtom::New(aEnvironment, std::to_string(op->iRightPrecedence));
}

void LispIsPreFix(LispEnvironment& aEnvironment, int aStackTop)
{
  LispInFixOperator* op = OperatorInfo(aEnvironment,
                                         aStackTop,
                                         aEnvironment.PreFix());
  InternalBoolean(aEnvironment, RESULT, op != nullptr);
}

void LispIsPostFix(LispEnvironment& aEnvironment, int aStackTop)
{
  LispInFixOperator* op = OperatorInfo(aEnvironment,
                                       aStackTop,
                                       aEnvironment.PostFix());

  InternalBoolean(aEnvironment, RESULT, op != nullptr);
}

void YacasBuiltinPrecisionGet(LispEnvironment& aEnvironment, int aStackTop)
{
    RESULT = LispAtom::New(aEnvironment, std::to_string(aEnvironment.Precision()));
}

void LispToString(LispEnvironment& aEnvironment, int aStackTop)
{
    std::ostringstream os;

    LispLocalOutput localOutput(aEnvironment, os);

    // Evaluate the body
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));

    //Return the result
    RESULT = LispAtom::New(aEnvironment, stringify(os.str()));
}

void LispToStdout(LispEnvironment& aEnvironment, int aStackTop)
{
    LispLocalOutput localOutput(aEnvironment, *aEnvironment.iInitialOutput);
    // Evaluate the body
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
}

void LispSecure(LispEnvironment& aEnvironment,int aStackTop)
{
    LispSecureFrame security(aEnvironment);
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
}

void LispFindFile(LispEnvironment& aEnvironment,int aStackTop)
{
    CheckSecure(aEnvironment, aStackTop);

    LispPtr evaluated(ARGUMENT(1));

    // Get file name
    CheckArg(evaluated, 1, aEnvironment, aStackTop);
    const LispString* orig = evaluated->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    const std::string oper = InternalUnstringify(*orig);

    const std::string path =
            InternalFindFile(oper.c_str(), aEnvironment.iInputDirectories);

    RESULT = LispAtom::New(aEnvironment, stringify(path));
}

void LispIsGeneric(LispEnvironment& aEnvironment,int aStackTop)
{
  LispPtr evaluated(ARGUMENT(1));

  InternalBoolean(aEnvironment, RESULT, evaluated->Generic() != nullptr);
}

void LispGenericTypeName(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));
    CheckArg(evaluated, 1, aEnvironment, aStackTop);

    const char* name = evaluated->Generic()->TypeName();
    RESULT = (LispAtom::New(aEnvironment,name));
}

void GenArrayCreate(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr sizearg(ARGUMENT(1));

    CheckArg(sizearg, 1, aEnvironment, aStackTop);
    CheckArg(sizearg->String(), 1, aEnvironment, aStackTop);

    int size = InternalAsciiToInt(*sizearg->String());

    LispPtr initarg(ARGUMENT(2));

    ArrayClass *array = new ArrayClass(size,initarg);
    RESULT = (LispGenericClass::New(array));
}

void GenArraySize(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    GenericClass *gen = evaluated->Generic();
    ArrayClass* arr = dynamic_cast<ArrayClass*>(gen);
    CheckArg(arr, 1, aEnvironment, aStackTop);
    RESULT = LispAtom::New(aEnvironment, std::to_string(arr->Size()));
}

void GenArrayGet(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    GenericClass *gen = evaluated->Generic();
    ArrayClass* arr = dynamic_cast<ArrayClass*>(gen);
    CheckArg(arr, 1, aEnvironment, aStackTop);

    LispPtr sizearg(ARGUMENT(2));
    CheckArg(sizearg, 2, aEnvironment, aStackTop);
    CheckArg(sizearg->String(), 2, aEnvironment, aStackTop);

    int size = InternalAsciiToInt(*sizearg->String());

    CheckArg(size > 0 && static_cast<std::size_t>(size) <= arr->Size(), 2, aEnvironment, aStackTop);
    LispObject* object = arr->GetElement(size);
    RESULT = (object->Copy());
}

void GenArraySet(LispEnvironment& aEnvironment,int aStackTop)
{
  LispPtr evaluated(ARGUMENT(1));

  GenericClass *gen = evaluated->Generic();
  ArrayClass* arr = dynamic_cast<ArrayClass*>(gen);
  CheckArg(arr, 1, aEnvironment, aStackTop);

  LispPtr sizearg(ARGUMENT(2));
  CheckArg(sizearg, 2, aEnvironment, aStackTop);
  CheckArg(sizearg->String(), 2, aEnvironment, aStackTop);

  int size = InternalAsciiToInt(*sizearg->String());

  CheckArg(size > 0 && static_cast<std::size_t>(size) <= arr->Size(), 2, aEnvironment, aStackTop);
  LispPtr obj(ARGUMENT(3));
  arr->SetElement(size,obj);

  InternalTrue( aEnvironment, RESULT);
}

void GenAssociationCreate(LispEnvironment& aEnvironment,int aStackTop)
{
    AssociationClass* a = new AssociationClass(aEnvironment);
    RESULT = LispGenericClass::New(a);
}

void GenAssociationSize(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));

    GenericClass *gen = evaluated->Generic();
    AssociationClass* a = dynamic_cast<AssociationClass*>(gen);
    CheckArg(a, 1, aEnvironment, aStackTop);
    RESULT = LispAtom::New(aEnvironment, std::to_string(a->Size()));
}

void GenAssociationContains(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr p(ARGUMENT(1));
    GenericClass* gen = p->Generic();
    AssociationClass* a = dynamic_cast<AssociationClass*>(gen);
    CheckArg(a, 1, aEnvironment, aStackTop);

    LispPtr k(ARGUMENT(2));

    if (a->GetElement(k))
        InternalTrue(aEnvironment,RESULT);
    else
        InternalFalse(aEnvironment,RESULT);
}

void GenAssociationGet(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr p(ARGUMENT(1));
    GenericClass* gen = p->Generic();
    AssociationClass* a = dynamic_cast<AssociationClass*>(gen);
    CheckArg(a, 1, aEnvironment, aStackTop);

    LispPtr k(ARGUMENT(2));
    LispObject* v = a->GetElement(k);

    if (v)
        RESULT = v->Copy();
    else
        RESULT = LispAtom::New(aEnvironment, "Undefined");
}

void GenAssociationSet(LispEnvironment& aEnvironment,int aStackTop)
{
  LispPtr p(ARGUMENT(1));
  GenericClass* gen = p->Generic();
  AssociationClass* a = dynamic_cast<AssociationClass*>(gen);
  CheckArg(a, 1, aEnvironment, aStackTop);

  LispPtr k(ARGUMENT(2));
  LispPtr v(ARGUMENT(3));

  a->SetElement(k, v);

  InternalTrue( aEnvironment, RESULT);
}

void GenAssociationDrop(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr p(ARGUMENT(1));
    GenericClass* gen = p->Generic();
    AssociationClass* a = dynamic_cast<AssociationClass*>(gen);
    CheckArg(a, 1, aEnvironment, aStackTop);

    LispPtr k(ARGUMENT(2));
    if (a->DropElement(k))
        InternalTrue(aEnvironment,RESULT);
    else
        InternalFalse(aEnvironment,RESULT);
}

void GenAssociationKeys(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr p(ARGUMENT(1));
    GenericClass* gen = p->Generic();
    AssociationClass* a = dynamic_cast<AssociationClass*>(gen);
    CheckArg(a, 1, aEnvironment, aStackTop);

    RESULT = a->Keys();
}

void GenAssociationToList(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr p(ARGUMENT(1));
    GenericClass* gen = p->Generic();
    AssociationClass* a = dynamic_cast<AssociationClass*>(gen);
    CheckArg(a, 1, aEnvironment, aStackTop);

    RESULT = a->ToList();
}

void GenAssociationHead(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr p(ARGUMENT(1));
    GenericClass* gen = p->Generic();
    AssociationClass* a = dynamic_cast<AssociationClass*>(gen);
    CheckArg(a, 1, aEnvironment, aStackTop);
    CheckArg(a->Size(), 1, aEnvironment, aStackTop);

    RESULT = a->Head();
}

void LispCustomEval(LispEnvironment& aEnvironment,int aStackTop)
{
  if (aEnvironment.iDebugger) delete aEnvironment.iDebugger;
  aEnvironment.iDebugger = new DefaultDebugger(ARGUMENT(1), ARGUMENT(2),ARGUMENT(3));
  LispLocalEvaluator local(aEnvironment,new TracedEvaluator);
  aEnvironment.iDebugger->Start();
  InternalEval(aEnvironment, RESULT, ARGUMENT(4));
  aEnvironment.iDebugger->Finish();
  delete aEnvironment.iDebugger;
  aEnvironment.iDebugger = nullptr;
}

void LispCustomEvalExpression(LispEnvironment& aEnvironment,int aStackTop)
{
  if (!aEnvironment.iDebugger)
      throw LispErrGeneric("Trying to get CustomEval results while not in custom evaluation");

  RESULT = (aEnvironment.iDebugger->iTopExpr);
}

void LispCustomEvalResult(LispEnvironment& aEnvironment,int aStackTop)
{
  if (!aEnvironment.iDebugger)
      throw LispErrGeneric("Trying to get CustomEval results while not in custom evaluation");

  RESULT = (aEnvironment.iDebugger->iTopResult);
}

void LispCustomEvalLocals(LispEnvironment& aEnvironment,int aStackTop)
{
  aEnvironment.CurrentLocals(RESULT);
}

void LispCustomEvalStop(LispEnvironment& aEnvironment,int aStackTop)
{
  if (!aEnvironment.iDebugger)
      throw LispErrGeneric("Trying to get CustomEval results while not in custom evaluation");

  aEnvironment.iDebugger->iStopped = true;

  InternalTrue(aEnvironment,RESULT);
}

void LispTraceStack(LispEnvironment& aEnvironment,int aStackTop)
{
    LispLocalEvaluator local(aEnvironment,new TracedStackEvaluator);
    InternalEval(aEnvironment, RESULT, ARGUMENT(1));
}

void LispReadLisp(LispEnvironment& aEnvironment, int aStackTop)
{
    LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
    LispParser parser(tok,
                      *aEnvironment.CurrentInput(),
                      aEnvironment);
    // Read expression
    parser.Parse(RESULT);
}

void LispReadLispListed(LispEnvironment& aEnvironment, int aStackTop)
{
    LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
    LispParser parser(tok,
                      *aEnvironment.CurrentInput(),
                      aEnvironment);
    parser.iListed = true;
    // Read expression
    parser.Parse(RESULT);
}

void LispTraceRule(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr *ptr = ARGUMENT(0)->Nixed()->SubList();
    LispUserFunction* userfunc=nullptr;
    if (ptr)
        userfunc = GetUserFunction(aEnvironment,ptr);
    LispLocalTrace trace(userfunc);
    InternalEval(aEnvironment, RESULT, ARGUMENT(2));
}

void LispType(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr evaluated(ARGUMENT(1));
    LispPtr* subList = evaluated->SubList();
    LispObject* head = nullptr;
    if (!subList)
    {
        goto EMPTY;
    }
    head = (*subList);
    if (!head->String())
        goto EMPTY;
    RESULT = LispAtom::New(aEnvironment, *aEnvironment.HashTable().LookUp(stringify(*head->String())));
    return;

EMPTY:
    RESULT = LispAtom::New(aEnvironment, "\"\"");
    return;
}

void YacasStringMidGet(LispEnvironment& aEnvironment,int aStackTop)
{
    CheckArgIsString(3, aEnvironment, aStackTop);
    LispPtr evaluated(ARGUMENT(3));

    const LispString* orig = evaluated->String();

    LispPtr index(ARGUMENT(1));
    CheckArg(index, 1, aEnvironment, aStackTop);
    CheckArg(index->String(), 1, aEnvironment, aStackTop);
    const int sfrom = InternalAsciiToInt(*index->String());
    CheckArg(sfrom > 0, 1, aEnvironment, aStackTop);
    const std::size_t from = sfrom;

    index = (ARGUMENT(2));
    CheckArg(index, 2, aEnvironment, aStackTop);
    CheckArg(index->String(), 2, aEnvironment, aStackTop);
    const int scount = InternalAsciiToInt(*index->String());
    const std::size_t count = scount;

    std::string str = "\"";
    // FIXME: it's actually the set of args which is wrong, not the specific one
    CheckArg(from + count < orig->size(), 1, aEnvironment, aStackTop);
    for (std::size_t i = from; i < from + count; ++i)
        str.push_back((*orig)[i]);
    str.push_back('\"');
    RESULT = LispAtom::New(aEnvironment, str);
}

void YacasStringMidSet(LispEnvironment& aEnvironment,int aStackTop)
{
    CheckArgIsString(3, aEnvironment, aStackTop);
    LispPtr evaluated(ARGUMENT(3));
    const LispString* orig = evaluated->String();
    LispPtr index(ARGUMENT(1));
    CheckArg(index, 1, aEnvironment, aStackTop);
    CheckArg(index->String(), 1, aEnvironment, aStackTop);
    const int sfrom = InternalAsciiToInt(*index->String());
    CheckArg(sfrom > 0, 1, aEnvironment, aStackTop);
    const std::size_t from = sfrom;

    LispPtr ev2(ARGUMENT(2));
    CheckArgIsString(2, aEnvironment, aStackTop);
    const LispString* replace = ev2->String();

    std::string str(*orig);
    const std::size_t count = replace->size();
    // FIXME: it's actually the set of args which is wrong, not the specific one
    CheckArg(from + count < orig->size() + 2, 1, aEnvironment, aStackTop);

    for (std::size_t i = 0; i < count - 2; ++i)
        str[i+from] = (*replace)[i+1];
    RESULT = LispAtom::New(aEnvironment, str);
}

void LispFindFunction(LispEnvironment& aEnvironment,int aStackTop)
{
    CheckSecure(aEnvironment, aStackTop);

    LispPtr evaluated(ARGUMENT(1));

    // Get file name
    CheckArg(evaluated, 1, aEnvironment, aStackTop);
    const LispString* orig = evaluated->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    const std::string oper = InternalUnstringify(*orig);

    LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper));
    if (multiUserFunc)
    {
        LispDefFile* def = multiUserFunc->iFileToOpen;
        if (def)
        {
            RESULT = LispAtom::New(aEnvironment, def->FileName());
            return;
        }
    }

    RESULT = LispAtom::New(aEnvironment,"\"\"");
}

/// Corresponds to the Yacas function \c PatternCreate .
/// This function constructs a new PatternClass, and puts it in a new
/// LispGenericObject. The result is set to this LispGenericObject.
void GenPatternCreate(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr pattern(ARGUMENT(1));
    LispPtr postpredicate(ARGUMENT(2));

    LispIterator iter(pattern);
    LispObject * pObj = iter.getObj();
    CheckArg(pObj, 1, aEnvironment, aStackTop);
    LispPtr * pPtr = pObj->SubList();
    CheckArg(pPtr, 1, aEnvironment, aStackTop);
    iter = *pPtr;
    CheckArg(iter.getObj(), 1, aEnvironment, aStackTop);
    ++iter;

    YacasPatternPredicateBase* matcher =
        new YacasPatternPredicateBase(aEnvironment, *iter,postpredicate);
    PatternClass *p = new PatternClass(matcher);
    RESULT = (LispGenericClass::New(p));
}

void GenPatternMatches(LispEnvironment& aEnvironment,int aStackTop)
{
  LispPtr pattern(ARGUMENT(1));
  GenericClass *gen = pattern->Generic();
  PatternClass* pat = dynamic_cast<PatternClass*>(gen);
  CheckArg(pat, 1, aEnvironment, aStackTop);

  LispPtr list(ARGUMENT(2));

  LispIterator iter(list);
  LispObject * pObj = iter.getObj();
  CheckArg(pObj, 2, aEnvironment, aStackTop);
  LispPtr * pPtr = pObj->SubList();
  CheckArg(pPtr, 2, aEnvironment, aStackTop);
  iter = *pPtr;
  CheckArg(iter.getObj(), 2, aEnvironment, aStackTop);
  ++iter;

  CheckArg(iter.getObj(), 2, aEnvironment, aStackTop);
  bool matches = pat->Matches(aEnvironment,*iter);
  InternalBoolean(aEnvironment,RESULT,matches);
}

void LispRuleBaseDefined(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr name(ARGUMENT(1));
    const LispString* orig = name->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    const std::string oper = InternalUnstringify(*orig);

    LispPtr sizearg(ARGUMENT(2));
    CheckArg(sizearg, 2, aEnvironment, aStackTop);
    CheckArg(sizearg->String(), 2, aEnvironment, aStackTop);

    int arity = InternalAsciiToInt(*sizearg->String());

    LispUserFunction* userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper),arity);
    InternalBoolean(aEnvironment,RESULT,!!userFunc);
}

void LispDefLoadFunction(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr name(ARGUMENT(1));
    const LispString* orig = name->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    const std::string oper = InternalUnstringify(*orig);

    LispMultiUserFunction* multiUserFunc =
        aEnvironment.MultiUserFunction(aEnvironment.HashTable().LookUp(oper));
    if (multiUserFunc)
    {
        if (multiUserFunc->iFileToOpen!=nullptr)
        {
            LispDefFile* def = multiUserFunc->iFileToOpen;
            if (!def->IsLoaded())
            {
                multiUserFunc->iFileToOpen=nullptr;
                //InternalUse(aEnvironment, def->FileName());
      }
        }
    }
    InternalTrue(aEnvironment,RESULT);
}

void LispRuleBaseArgList(LispEnvironment& aEnvironment,int aStackTop)
{
    LispPtr name(ARGUMENT(1));
    const LispString* orig = name->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    const std::string oper = InternalUnstringify(*orig);

    LispPtr sizearg(ARGUMENT(2));
    CheckArg(sizearg, 2, aEnvironment, aStackTop);
    CheckArg(sizearg->String(), 2, aEnvironment, aStackTop);

    int arity = InternalAsciiToInt(*sizearg->String());

    LispUserFunction* userFunc = aEnvironment.UserFunction(aEnvironment.HashTable().LookUp(oper),arity);
    CheckArg(userFunc, 1, aEnvironment, aStackTop);

    const LispPtr& list = userFunc->ArgList();
    LispPtr head(aEnvironment.iList->Copy());
    head->Nixed() = (list);
    RESULT = (LispSubList::New(head));
}

static void InternalNewRulePattern(LispEnvironment& aEnvironment, int aStackTop, bool aMacroMode)
{
    int arity;
    int precedence;

    LispPtr ar;
    LispPtr pr;
    LispPtr predicate;
    LispPtr body;

    // Get operator
    CheckArg(ARGUMENT(1), 1, aEnvironment, aStackTop);
    const LispString* orig = ARGUMENT(1)->String();
    CheckArg(orig, 1, aEnvironment, aStackTop);
    ar = (ARGUMENT(2));
    pr = (ARGUMENT(3));
    predicate = (ARGUMENT(4));
    body = (ARGUMENT(5));

    // The arity
    CheckArg(ar, 2, aEnvironment, aStackTop);
    CheckArg(ar->String(), 2, aEnvironment, aStackTop);
    arity = InternalAsciiToInt(*ar->String());

    // The precedence
    CheckArg(ar, 3, aEnvironment, aStackTop);
    CheckArg(ar->String(), 3, aEnvironment, aStackTop);
    precedence = InternalAsciiToInt(*pr->String());

    // Finally define the rule base
    aEnvironment.DefineRulePattern(SymbolName(aEnvironment,*orig),
                                   arity,
                                   precedence,
                                   predicate,
                                   body);

    // Return true
    InternalTrue(aEnvironment,RESULT);
}

void LispNewRulePattern(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalNewRulePattern(aEnvironment, aStackTop, false);
}

void LispMacroNewRulePattern(LispEnvironment& aEnvironment, int aStackTop)
{
    InternalNewRulePattern(aEnvironment, aStackTop, true);
}
