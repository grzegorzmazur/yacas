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
    if (!(*aPtr.Get()->SubList()->Get()->String() == "List"))
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
                       aEnvironment.HashTable(),
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

    LispStringPtr hashedname = aEnvironment.HashTable().LookUp(oper.String());
    LispRamFile* ramFile=aEnvironment.iRamDisk.LookUp(hashedname);

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname->String());

    if (ramFile != NULL)
    {
        StringInput newInput(*(ramFile->Contents()),aEnvironment.iInputStatus);
        DoInternalLoad(aEnvironment,&newInput);
    }
    else
    {
        //TODO make the file api platform independent!!!!
        // Open file
        LispLocalFile localFP(aEnvironment, oper.String(),LispTrue,
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

    LispAtom *head =
        LispAtom::New(aEnvironment.HashTable().LookUpUnStringify(aOperator->String()));
    head->Next().Set(aArgs.Get());
    LispPtr body;
    body.Set(LispSubList::New(head));
    InternalEval(aEnvironment, aResult, body);
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
                       aEnvironment.HashTable(),
                       aEnvironment.PreFix(),
                       aEnvironment.InFix(),
                       aEnvironment.PostFix(),
                       aEnvironment.Bodied());
    parser.Parse(lispexpr);

    InternalEval(aEnvironment, aResult, lispexpr);
}


LispInt StrCompare(LispCharPtr f1, LispCharPtr f2)
{
    while (*f1)
    {
        /* f1 > f2  */
        if ( (*f2) == '\0')
            return 1;
        if (*f1 < *f2)
            return -1;
        else if (*f1 > *f2)
            return 1;
        f1++;
        f2++;
    }

    /* f1 < f2  */
    if (*f2)
        return -1;
    /* f1 = f2 */
    return 0;
}

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
                       aEnvironment.HashTable(),
                       aEnvironment.PreFix(),
                       aEnvironment.InFix(),
                       aEnvironment.PostFix(),
                       aEnvironment.Bodied());
    parser.Parse(aResult);
}



