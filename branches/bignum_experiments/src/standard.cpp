// \file standard.cpp
// Implementation of some standard lisp operations
//
#include "yacasprivate.h"
#include "standard.h"
#include "lispatom.h"
#include "lisperror.h"
#include "lispenvironment.h"

#include "lispio.h"
#include "platfileio.h"
#include "mathutil.h"
#include "lispenvironment.h"
#include "tokenizer.h"
#include "infixparser.h"
#include "lispeval.h"
#include "stringio.h"
#include "numbers.h"

#define InternalEval aEnvironment.iEvaluator->Eval


LispBoolean InternalIsList(LispPtr& aPtr)
{
    if (!aPtr)
        return LispFalse;
    if (!aPtr->SubList())
        return LispFalse;
    if (!(*aPtr->SubList()))
        return LispFalse;
    // The following happens with IsList(UnList({foo(x), a, b}))
    if ((*aPtr->SubList())->String()->c_str() == NULL)
        return LispFalse;
    //TODO this StrEqual is far from perfect. We could pass in a LispEnvironment object...
    if (!(StrEqual((*aPtr->SubList())->String()->c_str(), "List")))
        return LispFalse;
    return LispTrue;
}


LispBoolean InternalIsString(LispString * aOriginal)
{
    if (aOriginal)
        if ((*aOriginal)[0] == '\"')
            if ((*aOriginal)[aOriginal->Size()-2] == '\"')
                return LispTrue;
    return LispFalse;
}


/* TODO: in documenting the choices made in the C++ engine, perhaps document why I pass objects that should contain the
   result by reference. Result string passed in by reference to avoid copy-constructors etcetera (allowing
   the code to share the same LispString in different places).
 */
void InternalUnstringify(LispString& aResult, LispString * aOriginal)
{
  /*TODO: should these not be checked also higher up, and should this not be an assert at this level?
   * ideally this function should be as efficient as possible (allowing for a code generator to generate
   * compiled code that calls this function immediately. The compiler could prove that the input is valid,
   * so the checks would not be needed here in a non-debug run).
   *
   * Also do not forget to make the change in the Java version then, and find the other places where this is relevant.
   */
  Check(aOriginal,KLispErrInvalidArg);
  Check((*aOriginal)[0] == '\"',KLispErrInvalidArg);
  LispInt nrc = aOriginal->Size()-2;
  Check((*aOriginal)[nrc] == '\"',KLispErrInvalidArg);

  aResult.ResizeTo(nrc);
  for (LispInt i = 1; i < nrc; i++)
    aResult[i-1] = (*aOriginal)[i];
  aResult[nrc-1]='\0';
}

void InternalStringify(LispString& aResult, LispString * aOriginal)
{
    Check(aOriginal,KLispErrInvalidArg);

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
LispInt InternalAsciiToInt(LispString * aString)
{
  LispChar * ptr = aString->c_str();
  Check(IsNumber(ptr,LispFalse),KLispErrInvalidArg);
  return PlatAsciiToInt(ptr);
}

LispBoolean IsNumber(const LispChar * ptr,LispBoolean aAllowFloat)
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
            return LispFalse;
        index++;
        while(ptr[index] >= '0' && ptr[index] <= '9')
        {
            nrDigits++;
            index++;
        }
    }
    if (nrDigits == 0)
        return LispFalse;
    if (ptr[index] == 'e' || ptr[index] == 'E')
    {
        if (!aAllowFloat)
            return LispFalse;
        if (!NumericSupportForMantissa())
            return LispFalse;
        index++;
        if (ptr[index] == '-' || ptr[index] == '+') index++;
        while(ptr[index] >= '0' && ptr[index] <= '9') index++;
    }
    if (ptr[index] != '\0') return LispFalse;
    return LispTrue;
}


void InternalNth(LispPtr& aResult, LispPtr& aArg, LispInt n)
{
    Check(!!aArg,KLispErrInvalidArg);
    Check(aArg->SubList(),KLispErrInvalidArg);
    Check(n>=0,KLispErrInvalidArg);
    LispIterator iter(*aArg->SubList());

    while (n>0)
    {
        Check(iter.getObj(),KLispErrInvalidArg);
        ++iter;
        n--;
    }
    Check(iter.getObj(),KLispErrInvalidArg);
    aResult = (iter.getObj()->Copy());
}

void InternalTail(LispPtr& aResult, LispPtr& aArg)
{
    Check(!!aArg,KLispErrInvalidArg);

    LispPtr* iter = aArg->SubList();

    Check(iter,KLispErrInvalidArg);
    Check(!!(*iter),KLispErrInvalidArg);
    aResult = (LispSubList::New((*iter)->Nixed()));
}



void InternalReverseList(LispPtr& aResult, LispPtr& aOriginal)
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

void InternalFlatCopy(LispPtr& aResult, LispPtr& aOriginal)
{
    LispIterator orig(aOriginal);
    LispIterator res(aResult);

    while (orig.getObj())
    {
        (*res) = (orig.getObj()->Copy());
        ++orig;
        ++res;
    }
}

LispInt InternalListLength(LispPtr& aOriginal)
{
    LispIterator iter(aOriginal);
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

LispBoolean InternalEquals(LispEnvironment& aEnvironment,
                           LispPtr& aExpression1,
                           LispPtr& aExpression2)
{
    // Handle pointers to same, or NULL
    if (aExpression1.ptr() == aExpression2.ptr())  // compare pointers to LispObject
    {
        return LispTrue;
    }


/*TODO This code would be better, if BigNumber::Equals works*/

    BigNumber *n1 = aExpression1->Number(aEnvironment.Precision());
    BigNumber *n2 = aExpression2->Number(aEnvironment.Precision());
    if (!(!n1 && !n2) )
    {
        if (n1 == n2)
        {
            return LispTrue;
        }
        if (!n1) return LispFalse;
        if (!n2) return LispFalse;
        if (n1->Equals(*n2)) return LispTrue;
//this should be enabled
        return LispFalse;
    }


    //Pointers to strings should be the same
    if (aExpression1->String() != aExpression2->String())
    {
        return LispFalse;
    }

    // Handle same sublists, or NULL
    if (aExpression1->SubList() == aExpression2->SubList())
    {
        return LispTrue;
    }

    // Now check the sublists
    if (aExpression1->SubList())
    {
        if (!aExpression2->SubList())
        {
            return LispFalse;
        }
        LispIterator iter1(*aExpression1->SubList());
        LispIterator iter2(*aExpression2->SubList());

        while (iter1.getObj() && iter2.getObj())
        {
            // compare two list elements
            if (!InternalEquals(aEnvironment, *iter1, *iter2))
            {
                return LispFalse;
            }
 
            // Step to next
            ++iter1;
            ++iter2;
        }
        // Lists don't have the same length
        if (iter1.getObj() != iter2.getObj())
            return LispFalse;

        // Same!
        return LispTrue;
    }

    // expressions sublists are not the same!
    return LispFalse;
}
 
void DoInternalLoad(LispEnvironment& aEnvironment,LispInput* aInput)
{
    LispLocalInput localInput(aEnvironment, aInput);

    // TODO make "EndOfFile" a global thing
    // read-parse-eval to the end of file
    LispString * eof = aEnvironment.HashTable().LookUp("EndOfFile");
    LispBoolean endoffile = LispFalse;

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

        Check(!!readIn, KLispErrReadingFile);
        // Check for end of file
        if (readIn->String() == eof)
        {
            endoffile = LispTrue;
            }
        // Else evaluate
        else
        {
            LispPtr result;
            InternalEval(aEnvironment, result, readIn);
        }
    }
}

void InternalLoad(LispEnvironment& aEnvironment,LispString * aFileName)
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
        LispLocalFile localFP(aEnvironment, hashedname->c_str(),LispTrue,
                              aEnvironment.iInputDirectories);
        Check(localFP.iOpened != 0, KLispErrFileNotFound);
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
    Check(InternalIsString(aOperator),KLispErrNotString);

    LispObject *head =
        LispAtom::New(aEnvironment,SymbolName(aEnvironment, aOperator->c_str())->c_str());
    head->Nixed() = (aArgs);
    LispPtr body(LispSubList::New(head));
    InternalEval(aEnvironment, aResult, body);
}

void InternalApplyPure(LispPtr& oper,LispPtr& args2,LispPtr& aResult,LispEnvironment& aEnvironment)
{
//LispString text;
//    {
//        PrintExpression(text, oper,aEnvironment, 80);
//        printf("oper %s\n",text.String());
//
//    }
//printf("0...\n");
  LispPtr * chk1 = oper->SubList();
    Check(chk1,KLispErrInvalidArg);
    Check(!!(*chk1),KLispErrInvalidArg);
    LispPtr oper2((*chk1)->Nixed());
    Check(!!oper2,KLispErrInvalidArg);
//printf("1...\n");

    LispPtr body(oper2->Nixed());
    Check(!!body,KLispErrInvalidArg);

//printf("2...\n");
  LispPtr * chk2 = oper2->SubList();
    Check(chk2,KLispErrInvalidArg);
    Check(!!(*chk2),KLispErrInvalidArg);
    oper2 = ((*chk2)->Nixed());

//printf("3...\n");
    LispLocalFrame frame(aEnvironment,LispFalse);

//printf("4...\n");
    while (!!oper2)
    {
//printf("4.1..\n");
        Check(!!args2,KLispErrInvalidArg);

//printf("4.2..\n");
        LispString * var = oper2->String();
        Check(var,KLispErrInvalidArg);

//PrintExpression(text, args2,aEnvironment, 80);
//printf("arg %s\n",text.String());
//printf("4.3..\n");
        LispPtr newly(args2->Copy());
//printf("4.4..\n");
        aEnvironment.NewLocal(var,newly);

//printf("4.5..\n");
        oper2 = (oper2->Nixed());
//printf("4.6..\n");
        args2 = (args2->Nixed());
//printf("4.7..\n");
    }
//printf("5...\n");

    Check(!args2,KLispErrInvalidArg);
//printf("Before eval\n");
    InternalEval(aEnvironment, aResult, body);

//    {
//        PrintExpression(text, aResult,aEnvironment, 80);
//        printf("result %s\n",text.String());
//
//    }

}


void InternalEvalString(LispEnvironment& aEnvironment, LispPtr& aResult,
                        LispChar * aString)
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

    InternalEval(aEnvironment, aResult, lispexpr);
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

void ParseExpression(LispPtr& aResult,LispChar * aString,LispEnvironment& aEnvironment)
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

void PrintExpression(LispString& aResult, LispPtr& aExpression,
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

LispString * SymbolName(LispEnvironment& aEnvironment,
                         LispChar * aSymbol)
{
    if (aSymbol[0] == '\"')
    {
        return aEnvironment.HashTable().LookUpUnStringify(aSymbol);
    }
    else
    {
        return aEnvironment.HashTable().LookUp(aSymbol);
    }
}


