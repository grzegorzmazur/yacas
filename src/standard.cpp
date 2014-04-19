// \file standard.cpp
// Implementation of some standard lisp operations
//
#include "yacas/yacasprivate.h"
#include "yacas/standard.h"
#include "yacas/lispatom.h"
#include "yacas/lisperror.h"
#include "yacas/lispenvironment.h"

#include "yacas/lispio.h"
#include "yacas/platfileio.h"
#include "yacas/mathutil.h"
#include "yacas/lispenvironment.h"
#include "yacas/tokenizer.h"
#include "yacas/infixparser.h"
#include "yacas/lispeval.h"
#include "yacas/stringio.h"
#include "yacas/numbers.h"

bool InternalIsList(const LispPtr& aPtr)
{
    if (!aPtr)
        return false;
    if (!aPtr->SubList())
        return false;
    if (!(*aPtr->SubList()))
        return false;
    // The following happens with IsList(UnList({foo(x), a, b}))
    if ((*aPtr->SubList())->String()->c_str() == NULL)
        return false;
    //TODO this StrEqual is far from perfect. We could pass in a LispEnvironment object...
    if (std::strcmp((*aPtr->SubList())->String()->c_str(), "List"))
        return false;
    return true;
}


bool InternalIsString(LispString * aOriginal)
{
    if (aOriginal)
        if ((*aOriginal)[0] == '\"')
            if ((*aOriginal)[aOriginal->Size()-2] == '\"')
                return true;
    return false;
}


/* TODO: in documenting the choices made in the C++ engine, perhaps document why I pass objects that should contain the
   result by reference. Result string passed in by reference to avoid copy-constructors etcetera (allowing
   the code to share the same LispString in different places).
 */
void InternalUnstringify(LispString& aResult, const LispString * aOriginal)
{
  /*TODO: should these not be checked also higher up, and should this not be an assert at this level?
   * ideally this function should be as efficient as possible (allowing for a code generator to generate
   * compiled code that calls this function immediately. The compiler could prove that the input is valid,
   * so the checks would not be needed here in a non-debug run).
   *
   * Also do not forget to make the change in the Java version then, and find the other places where this is relevant.
   */
    if (!aOriginal || (*aOriginal)[0] != '\"')
        throw LispErrInvalidArg();

  LispInt nrc = aOriginal->Size()-2;

  if (!aOriginal || (*aOriginal)[nrc] != '\"')
      throw LispErrInvalidArg();

  aResult.ResizeTo(nrc);
  for (LispInt i = 1; i < nrc; i++)
    aResult[i-1] = (*aOriginal)[i];
  aResult[nrc-1]='\0';
}

void InternalStringify(LispString& aResult, const LispString* aOriginal)
{
    if (!aOriginal)
        throw LispErrInvalidArg();

    LispInt nrc=aOriginal->Size()-1;
    aResult.ResizeTo(nrc+3);
    LispInt i;
    aResult[0] = '\"';
    for (i=0;i<nrc;i++)
        aResult[i+1] = (*aOriginal)[i];
    aResult[nrc+1] = '\"';
    aResult[nrc+2] = '\0';
}

void InternalIntToAscii(LispChar * aTrg,LispInt aInt)
{
    LispInt ind=0;
    if (aInt < 0)
    {
        *aTrg++ = '-';
        aInt = -aInt;
    }
    while (aInt != 0)
    {
        aTrg[ind] = '0'+(aInt%10);
        ind++;
        aInt/=10;
    }

    //zero
    if (ind == 0)
    {
        aTrg[0] = '0';
        ind++;
    }
    aTrg[ind]='\0';
    LispInt i;
    for (i=0;i<(ind>>1);i++)
    {
        LispChar swap=aTrg[i];
        aTrg[i] = aTrg[ind-i-1];
        aTrg[ind-i-1] = swap;
    }
}

// TODO: we should either pass the string by reference, or use an assert to check validity of input
LispInt InternalAsciiToInt(const LispString* aString)
{
  const LispChar * ptr = aString->c_str();

  if (!IsNumber(ptr, false))
      throw LispErrInvalidArg();

  return PlatAsciiToInt(ptr);
}

bool IsNumber(const LispChar * ptr,bool aAllowFloat)
{
    if (*ptr == '-' || *ptr == '+')
        ptr++;

    LispInt nrDigits=0;
    LispInt index=0;
    while(ptr[index] >= '0' && ptr[index] <= '9')
    {
        nrDigits++;
        index++;
    }
    if (ptr[index] == '.')
    {
        if (!aAllowFloat)
            return false;
        index++;
        while(ptr[index] >= '0' && ptr[index] <= '9')
        {
            nrDigits++;
            index++;
        }
    }
    if (nrDigits == 0)
        return false;
    if (ptr[index] == 'e' || ptr[index] == 'E')
    {
        if (!aAllowFloat)
            return false;
        if (!NumericSupportForMantissa())
            return false;
        index++;
        if (ptr[index] == '-' || ptr[index] == '+') index++;
        while(ptr[index] >= '0' && ptr[index] <= '9') index++;
    }
    if (ptr[index] != '\0') return false;
    return true;
}


void InternalNth(LispPtr& aResult, const LispPtr& aArg, LispInt n)
{
    if (n < 0 || !aArg || !aArg->SubList())
        throw LispErrInvalidArg();

    LispIterator iter(*aArg->SubList());

    while (n>0)
    {
        if (!iter.getObj())
            throw LispErrInvalidArg();

        ++iter;
        n--;
    }

    if (!iter.getObj())
        throw LispErrInvalidArg();

    aResult = (iter.getObj()->Copy());
}

void InternalTail(LispPtr& aResult, const LispPtr& aArg)
{
    if (!aArg)
        throw LispErrInvalidArg();

    LispPtr* iter = aArg->SubList();

    if (!iter || !*iter)
        throw LispErrInvalidArg();

    aResult = (LispSubList::New((*iter)->Nixed()));
}



void InternalReverseList(LispPtr& aResult, const LispPtr& aOriginal)
{
    LispPtr iter(aOriginal);
    LispPtr previous;
    LispPtr tail(aOriginal);

    while (!!iter)
    {
        tail = iter->Nixed();
        iter->Nixed() = (previous);
        previous = iter;
        iter = tail;
    }
    aResult = previous;
}

void InternalFlatCopy(LispPtr& aResult, const LispPtr& aOriginal)
{
    LispConstIterator orig(aOriginal);
    LispIterator res(aResult);

    while (orig.getObj())
    {
        (*res) = (orig.getObj()->Copy());
        ++orig;
        ++res;
    }
}

LispInt InternalListLength(const LispPtr& aOriginal)
{
    LispConstIterator iter(aOriginal);
    LispInt length = 0;
    while (iter.getObj())
    {
        /*
         if (iter()->String())
            printf("%s ",iter()->String()->c_str());
         */
        ++iter;
        length++;
    }
    return length;
}

bool InternalEquals(LispEnvironment& aEnvironment,
                    const LispPtr& aExpression1,
                    const LispPtr& aExpression2)
{
    // Handle pointers to same, or NULL
    if (aExpression1.ptr() == aExpression2.ptr())  // compare pointers to LispObject
    {
        return true;
    }


/*TODO This code would be better, if BigNumber::Equals works*/

    BigNumber *n1 = aExpression1->Number(aEnvironment.Precision());
    BigNumber *n2 = aExpression2->Number(aEnvironment.Precision());
    if (!(!n1 && !n2) )
    {
        if (n1 == n2)
        {
            return true;
        }
        if (!n1) return false;
        if (!n2) return false;
        if (n1->Equals(*n2)) return true;
//this should be enabled
        return false;
    }


    //Pointers to strings should be the same
    if (aExpression1->String() != aExpression2->String())
    {
        return false;
    }

    // Handle same sublists, or NULL
    if (aExpression1->SubList() == aExpression2->SubList())
    {
        return true;
    }

    // Now check the sublists
    if (aExpression1->SubList())
    {
        if (!aExpression2->SubList())
        {
            return false;
        }
        LispIterator iter1(*aExpression1->SubList());
        LispIterator iter2(*aExpression2->SubList());

        while (iter1.getObj() && iter2.getObj())
        {
            // compare two list elements
            if (!InternalEquals(aEnvironment, *iter1, *iter2))
            {
                return false;
            }

            // Step to next
            ++iter1;
            ++iter2;
        }
        // Lists don't have the same length
        if (iter1.getObj() != iter2.getObj())
            return false;

        // Same!
        return true;
    }

    // expressions sublists are not the same!
    return false;
}

void DoInternalLoad(LispEnvironment& aEnvironment,LispInput* aInput)
{
    LispLocalInput localInput(aEnvironment, aInput);

    // TODO make "EndOfFile" a global thing
    // read-parse-eval to the end of file
    LispString * eof = aEnvironment.HashTable().LookUp("EndOfFile");
    bool endoffile = false;

    LispTokenizer tok;
    InfixParser parser(tok,
                       *aEnvironment.CurrentInput(),
                       aEnvironment,
                       aEnvironment.PreFix(),
                       aEnvironment.InFix(),
                       aEnvironment.PostFix(),
                       aEnvironment.Bodied());

    while (!endoffile)
    {
        LispPtr readIn;
        // Read expression
        parser.Parse(readIn);

        if (!readIn)
            throw LispErrReadingFile();

        // Check for end of file
        if (readIn->String() == eof)
        {
            endoffile = true;
            }
        // Else evaluate
        else
        {
            LispPtr result;
            aEnvironment.iEvaluator->Eval(aEnvironment, result, readIn);
        }
    }
}

void InternalLoad(LispEnvironment& aEnvironment, const LispString* aFileName)
{
    LispString oper;
    InternalUnstringify(oper, aFileName);

    LispString * contents = aEnvironment.FindCachedFile(oper.c_str());
    LispString * hashedname = aEnvironment.HashTable().LookUp(oper.c_str());

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname->c_str());

    if (contents)
    {
        StringInput newInput(*contents,aEnvironment.iInputStatus);
        DoInternalLoad(aEnvironment,&newInput);
        delete contents;
    }
    else
    {
        //TODO make the file api platform independent!!!!
        // Open file
        LispLocalFile localFP(aEnvironment, hashedname->c_str(),true,
                              aEnvironment.iInputDirectories);

        if (!localFP.iOpened)
            throw LispErrFileNotFound();

        FILEINPUT newInput(localFP,aEnvironment.iInputStatus);
        DoInternalLoad(aEnvironment,&newInput);
    }
    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
}

void InternalUse(LispEnvironment& aEnvironment,LispString * aFileName)
{
    LispDefFile* def = aEnvironment.DefFiles().File(aFileName);
    if (!def->IsLoaded())
    {
        def->SetLoaded();
        InternalLoad(aEnvironment,aFileName);
    }
}

void InternalApplyString(LispEnvironment& aEnvironment, LispPtr& aResult,
                 LispString * aOperator,LispPtr& aArgs)
{
    if (!InternalIsString(aOperator))
        throw LispErrNotString();

    LispObject *head =
        LispAtom::New(aEnvironment,SymbolName(aEnvironment, aOperator->c_str())->c_str());
    head->Nixed() = (aArgs);
    LispPtr body(LispSubList::New(head));
    aEnvironment.iEvaluator->Eval(aEnvironment, aResult, body);
}

void InternalApplyPure(LispPtr& oper,LispPtr& args2,LispPtr& aResult,LispEnvironment& aEnvironment)
{
    LispPtr * chk1 = oper->SubList();

    if (!chk1)
        throw LispErrInvalidArg();

    LispPtr oper2((*chk1)->Nixed());

    if (!oper2)
        throw LispErrInvalidArg();

    LispPtr body(oper2->Nixed());

    if (!body)
        throw LispErrInvalidArg();

    LispPtr * chk2 = oper2->SubList();

    if (!chk2 || !*chk2)
        throw LispErrInvalidArg();

    oper2 = ((*chk2)->Nixed());

    LispLocalFrame frame(aEnvironment,false);

    while (!!oper2)  {
        if (!args2)
            throw LispErrInvalidArg();

        LispString* var = oper2->String();

        if (!var)
            throw LispErrInvalidArg();

        LispPtr newly(args2->Copy());

        aEnvironment.NewLocal(var,newly);

        oper2 = (oper2->Nixed());

        args2 = (args2->Nixed());
    }

    if (args2)
        throw LispErrInvalidArg();

    aEnvironment.iEvaluator->Eval(aEnvironment, aResult, body);
}


void InternalEvalString(LispEnvironment& aEnvironment, LispPtr& aResult,
                        const LispChar* aString)
{
    LispString full(aString);
    full[full.Size()-1] = ';';
    full.Append('\0');
    StringInput input(full,aEnvironment.iInputStatus);
    LispPtr lispexpr;
    LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
    InfixParser parser(tok, input,
                       aEnvironment,
                       aEnvironment.PreFix(),
                       aEnvironment.InFix(),
                       aEnvironment.PostFix(),
                       aEnvironment.Bodied());
    parser.Parse(lispexpr);

    aEnvironment.iEvaluator->Eval(aEnvironment, aResult, lispexpr);
}

/*TODO put somewhere else? Platform-independent strcmp
LispInt PlatStrCompare(LispChar * f1, LispChar * f2)
{
    while (*f1)
    {
        // f1 > f2
        if ( (*f2) == '\0')
            return 1;
        if (*f1 < *f2)
            return -1;
        else if (*f1 > *f2)
            return 1;
        f1++;
        f2++;
    }

    // f1 < f2
    if (*f2)
        return -1;
    // f1 = f2
    return 0;
}
*/

LispObject* operator+(const LispObjectAdder& left, const LispObjectAdder& right)
{
  LispObject* trav = left.iPtr;
  while (!!trav->Nixed())
  {
        trav = trav->Nixed();
  }
  trav->Nixed() = (right.iPtr);
  return left.iPtr;
}

void ParseExpression(LispPtr& aResult, const LispChar* aString,LispEnvironment& aEnvironment)
{
    LispString full((LispChar *)aString);
    full[full.Size()-1] = ';';
    full.Append('\0');
    StringInput input(full,aEnvironment.iInputStatus);
    aEnvironment.iInputStatus.SetTo("String");
    LispTokenizer &tok = *aEnvironment.iCurrentTokenizer;
    InfixParser parser(tok,
                       input,
                       aEnvironment,
                       aEnvironment.PreFix(),
                       aEnvironment.InFix(),
                       aEnvironment.PostFix(),
                       aEnvironment.Bodied());
    parser.Parse(aResult);
}

void ReturnUnEvaluated(LispPtr& aResult,LispPtr& aArguments,
                       LispEnvironment& aEnvironment)
{
    LispPtr full(aArguments->Copy());
    aResult = (LispSubList::New(full));

    LispIterator iter(aArguments);
    ++iter;

    while (iter.getObj())
    {
        LispPtr next;
        aEnvironment.iEvaluator->Eval(aEnvironment, next, *iter);
        full->Nixed() = (next);
        full = (next);
        ++iter;
    }
    full->Nixed() = (NULL);
}

void PrintExpression(LispString& aResult,
                     LispPtr& aExpression,
                     LispEnvironment& aEnvironment,
                     LispInt aMaxChars)
{
    aResult.ResizeTo(0);
    aResult.Append('\0');
    StringOutput newOutput(aResult);
    InfixPrinter infixprinter(aEnvironment.PreFix(),
                              aEnvironment.InFix(),
                              aEnvironment.PostFix(),
                              aEnvironment.Bodied());
    infixprinter.Print(aExpression, newOutput, aEnvironment);
    if (aMaxChars > 0 && aResult.Size()>aMaxChars)
    {
        aResult[aMaxChars-3] = '.';
        aResult[aMaxChars-2] = '.';
        aResult[aMaxChars-1] = '.';
        aResult[aMaxChars] = '\0';
        aResult.ResizeTo(aMaxChars+1);
    }
}

LispString* SymbolName(LispEnvironment& aEnvironment,
                       const LispChar * aSymbol)
{
    if (aSymbol[0] == '\"')
        return aEnvironment.HashTable().LookUpCounted(aSymbol + 1, std::strlen(aSymbol) - 2);
    else
        return aEnvironment.HashTable().LookUp(aSymbol);
}


