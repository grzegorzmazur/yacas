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
    if (aPtr.Get() == NULL)
        return LispFalse;
    if (aPtr.Get()->SubList() == NULL)
        return LispFalse;
    if (aPtr.Get()->SubList()->Get() == NULL)
        return LispFalse;
    //TODO this StrEqual is far from perfect. We could pass in a LispEnvironment object...
    if (!(StrEqual(aPtr.Get()->SubList()->Get()->String()->String(), "List")))
        return LispFalse;
    return LispTrue;
}


LispBoolean InternalIsString(LispStringPtr aOriginal)
{
    if (aOriginal != NULL)
        if ((*aOriginal)[0] == '\"')
            if ((*aOriginal)[aOriginal->NrItems()-2] == '\"')
                return LispTrue;
    return LispFalse;
}



void InternalUnstringify(LispString& aResult, LispStringPtr aOriginal)
{
    Check(aOriginal != NULL,KLispErrInvalidArg);
    Check((*aOriginal)[0] == '\"',KLispErrInvalidArg);
    LispInt nrc=aOriginal->NrItems()-2;
    Check((*aOriginal)[nrc] == '\"',KLispErrInvalidArg);

    aResult.GrowTo(nrc);
    aResult.SetNrItems(nrc);
    LispInt i;
    for (i=1;i<nrc;i++)
        aResult[i-1] = (*aOriginal)[i];
    aResult[nrc-1]='\0';
}

void InternalStringify(LispString& aResult, LispStringPtr aOriginal)
{
    Check(aOriginal != NULL,KLispErrInvalidArg);

    LispInt nrc=aOriginal->NrItems()-1;
    aResult.GrowTo(nrc+3);
    aResult.SetNrItems(nrc+3);
    LispInt i;
    aResult[0] = '\"';
    for (i=0;i<nrc;i++)
        aResult[i+1] = (*aOriginal)[i];
    aResult[nrc+1] = '\"';
    aResult[nrc+2] = '\0';
}

void InternalIntToAscii(LispCharPtr aTrg,LispInt aInt)
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

LispInt InternalAsciiToInt(LispCharPtr aString)
{
    Check(IsNumber(aString,LispFalse),KLispErrInvalidArg);
    return PlatAsciiToInt(aString);
}


LispBoolean IsNumber(LispCharPtr ptr,LispBoolean aAllowFloat)
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
    Check(aArg.Get() != NULL,KLispErrInvalidArg);
    Check(aArg.Get()->SubList() != NULL,KLispErrInvalidArg);
    Check(n>=0,KLispErrInvalidArg);
    LispIterator iter(*aArg.Get()->SubList());

    while (n>0)
    {
        Check(iter() != NULL,KLispErrInvalidArg);
        iter.GoNext();
        n--;
    }
    Check(iter() != NULL,KLispErrInvalidArg);
    aResult.Set(iter()->Copy(0));
}

void InternalTail(LispPtr& aResult, LispPtr& aArg)
{
    Check(aArg.Get() != NULL,KLispErrInvalidArg);
    Check(aArg.Get()->SubList() != NULL,KLispErrInvalidArg);

    LispPtr* iter = aArg.Get()->SubList();

    Check(iter->Get() != NULL,KLispErrInvalidArg);
    aResult.Set(LispSubList::New(iter->Get()->Next().Get()));
}



void InternalReverseList(LispPtr& aResult, LispPtr& aOriginal)
{
    LispPtr iter(aOriginal);
    LispPtr previous;
    LispPtr tail = aOriginal;
    
    while (iter.Get())
    {
        tail = iter.Get()->Next();
        iter.Get()->Next().Set(previous.Get());
        previous = iter;
        iter = tail;
    }
    aResult = previous;
}

void InternalFlatCopy(LispPtr& aResult, LispPtr& aOriginal)
{
    LispIterator orig(aOriginal);
    LispIterator res(aResult);

    while (orig())
    {
        res.Ptr()->Set(orig()->Copy(LispFalse));
        orig.GoNext();
        res.GoNext();
    }
}

LispInt InternalListLength(LispPtr& aOriginal)
{
    LispIterator iter(aOriginal);
    LispInt length = 0;
    while (iter())
    {
        /*
         if (iter()->String())
            printf("%s ",iter()->String()->String());
         */
        iter.GoNext();
        length++;
    }
    return length;
}

LispBoolean InternalEquals(LispEnvironment& aEnvironment,
                           LispPtr& aExpression1,
                           LispPtr& aExpression2)
{
    // Handle pointers to same, or NULL
    if (aExpression1.Get() == aExpression2.Get())
    {
        return LispTrue;
    }


#ifndef NO_USE_BIGFLOAT
/*This code would be better, if BigNumber::Equals works*/

    BigNumber *n1 = aExpression1.Get()->Number(aEnvironment.Precision());
    BigNumber *n2 = aExpression2.Get()->Number(aEnvironment.Precision());
    if (!(n1 == NULL && n2 == NULL) )
    {
        if (n1 == n2)
        {
            return LispTrue;
        }
        if (n1 == NULL) return LispFalse;
        if (n2 == NULL) return LispFalse;
        if (n1->Equals(*n2)) return LispTrue;
//this should be enabled
        return LispFalse;
    }

#endif

    //Pointers to strings should be the same
    if (aExpression1.Get()->String() != aExpression2.Get()->String())
    {
        return LispFalse;
    }

    // Handle same sublists, or NULL
    if (aExpression1.Get()->SubList() == aExpression2.Get()->SubList())
    {
        return LispTrue;
    }

    // Now check the sublists
    if (aExpression1.Get()->SubList())
    {
        if (!aExpression2.Get()->SubList())
        {
            return LispFalse;
        }
        LispIterator iter1(*aExpression1.Get()->SubList());
        LispIterator iter2(*aExpression2.Get()->SubList());

        while (iter1() && iter2())
        {
            // compare two list elements
            if (!InternalEquals(aEnvironment, *iter1.Ptr(),*iter2.Ptr()))
            {
                return LispFalse;
            }
                
            // Step to next
            iter1.GoNext();
            iter2.GoNext();
        }
        // Lists don't have the same length
        if (iter1() != iter2())
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
    LispStringPtr eof = aEnvironment.HashTable().LookUp("EndOfFile");
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

        Check(readIn.Get() != NULL, KLispErrReadingFile);
        // Check for end of file
        if (readIn.Get()->String() == eof)
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

void InternalLoad(LispEnvironment& aEnvironment,LispStringPtr aFileName)
{
    LispString oper;
    InternalUnstringify(oper, aFileName);

    LispStringPtr contents = aEnvironment.FindCachedFile(oper.String());
    LispStringPtr hashedname = aEnvironment.HashTable().LookUp(oper.String());

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname->String());

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
        LispLocalFile localFP(aEnvironment, hashedname->String(),LispTrue,
                              aEnvironment.iInputDirectories);
        Check(localFP.iOpened != 0, KLispErrFileNotFound);
        FILEINPUT newInput(localFP,aEnvironment.iInputStatus);
        DoInternalLoad(aEnvironment,&newInput);
    }
    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
}
    
void InternalUse(LispEnvironment& aEnvironment,LispStringPtr aFileName)
{
    LispDefFile* def = aEnvironment.DefFiles().File(aFileName);
    if (!def->IsLoaded())
    {
        def->SetLoaded();
        InternalLoad(aEnvironment,aFileName);
    }
}

void InternalApplyString(LispEnvironment& aEnvironment, LispPtr& aResult,
                 LispStringPtr aOperator,LispPtr& aArgs)
{
    Check(InternalIsString(aOperator),KLispErrNotString);

    LispObject *head =
        LispAtom::New(aEnvironment,SymbolName(aEnvironment, aOperator->String())->String());
    head->Next().Set(aArgs.Get());
    LispPtr body;
    body.Set(LispSubList::New(head));
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
    Check(oper.Get()->SubList() != NULL,KLispErrInvalidArg);
    Check(oper.Get()->SubList()->Get() != NULL,KLispErrInvalidArg);
    LispPtr oper2;
    oper2.Set(oper.Get()->SubList()->Get()->Next().Get());
    Check(oper2.Get() != NULL,KLispErrInvalidArg);
//printf("1...\n");

    LispPtr body;
    body.Set(oper2.Get()->Next().Get());
    Check(body.Get() != NULL,KLispErrInvalidArg);

//printf("2...\n");
    Check(oper2.Get()->SubList() != NULL,KLispErrInvalidArg);
    Check(oper2.Get()->SubList()->Get() != NULL,KLispErrInvalidArg);
    oper2.Set(oper2.Get()->SubList()->Get()->Next().Get());

//printf("3...\n");
    LispLocalFrame frame(aEnvironment,LispFalse);

//printf("4...\n");
    while (oper2.Get() != NULL)
    {
//printf("4.1..\n");
        Check(args2.Get() != NULL,KLispErrInvalidArg);

//printf("4.2..\n");
        LispStringPtr var = oper2.Get()->String();
        Check(var != NULL,KLispErrInvalidArg);

//PrintExpression(text, args2,aEnvironment, 80);
//printf("arg %s\n",text.String());
//printf("4.3..\n");
        LispPtr newly;
        newly.Set(args2.Get()->Copy(LispFalse));
//printf("4.4..\n");
        aEnvironment.NewLocal(var,newly.Get());

//printf("4.5..\n");
        oper2.Set(oper2.Get()->Next().Get());
//printf("4.6..\n");
        args2.Set(args2.Get()->Next().Get());
//printf("4.7..\n");
    }
//printf("5...\n");

    Check(args2.Get() == NULL,KLispErrInvalidArg);
//printf("Before eval\n");
    InternalEval(aEnvironment, aResult, body);

//    {
//        PrintExpression(text, aResult,aEnvironment, 80);
//        printf("result %s\n",text.String());
//
//    }

}


void InternalEvalString(LispEnvironment& aEnvironment, LispPtr& aResult,
                        LispCharPtr aString)
{
    LispString full(aString);
    full[full.NrItems()-1] = ';';
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

/*TODO remove?
LispInt PlatStrCompare(LispCharPtr f1, LispCharPtr f2)
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
  while (trav->Next().Get() != NULL)
  {
        trav = trav->Next().Get();
  }
  trav->Next().Set(right.iPtr);
  return left.iPtr;
}

void ParseExpression(LispPtr& aResult,LispCharPtr aString,LispEnvironment& aEnvironment)
{
    LispString full((LispCharPtr)aString);
    full[full.NrItems()-1] = ';';
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
    LispPtr full;
    full.Set(aArguments.Get()->Copy(LispFalse));
    aResult.Set(LispSubList::New(full.Get()));

    LispIterator iter(aArguments);
    iter.GoNext();

    while (iter() != NULL)
    {
        LispPtr next;
        aEnvironment.iEvaluator->Eval(aEnvironment, next, *iter.Ptr());
        full.Get()->Next().Set(next.Get());
        full.Set(next.Get());
        iter.GoNext();
    }
    full.Get()->Next().Set(NULL);
}

void PrintExpression(LispString& aResult, LispPtr& aExpression,
                     LispEnvironment& aEnvironment,
                     LispInt aMaxChars)
{
    aResult.SetNrItems(0);
    aResult.Append('\0');
    StringOutput newOutput(aResult);
    InfixPrinter infixprinter(aEnvironment.PreFix(),
                              aEnvironment.InFix(),
                              aEnvironment.PostFix(),
                              aEnvironment.Bodied());
    infixprinter.Print(aExpression, newOutput, aEnvironment);
    if (aMaxChars > 0 && aResult.NrItems()>aMaxChars)
    {
        aResult[aMaxChars-3] = '.';
        aResult[aMaxChars-2] = '.';
        aResult[aMaxChars-1] = '.';
        aResult[aMaxChars] = '\0';
        aResult.SetNrItems(aMaxChars+1);
    }
}

LispStringPtr SymbolName(LispEnvironment& aEnvironment,
                         LispCharPtr aSymbol)
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


