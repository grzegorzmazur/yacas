
#include "yacasprivate.h"
#include "lispassert.h"
#include "infixparser.h"
#include "lispatom.h"
#include "standard.h"
#include "lisperror.h"



InfixParser::InfixParser(LispTokenizer& aTokenizer, LispInput& aInput,
                         LispHashTable& aHashTable,
                         LispOperators& aPrefixOperators,
                         LispOperators& aInfixOperators,
                         LispOperators& aPostfixOperators,
                         LispOperators& aBodiedOperators)
    : LispParser( aTokenizer,  aInput,aHashTable),
    iPrefixOperators(aPrefixOperators),
    iInfixOperators(aInfixOperators),
    iPostfixOperators(aPostfixOperators),
    iBodiedOperators(aBodiedOperators)
{
    iEndOfFile    = iHashTable.LookUp("EndOfFile");
    iEndStatement = iHashTable.LookUp(";");
    iProgOpen     = iHashTable.LookUp("[");
    iProgClose    = iHashTable.LookUp("]");
    iNth          = iHashTable.LookUp("Nth");
    iBracketOpen  = iHashTable.LookUp("(");
    iBracketClose = iHashTable.LookUp(")");
    iListOpen     = iHashTable.LookUp("{");
    iListClose    = iHashTable.LookUp("}");
    iComma        = iHashTable.LookUp(",");
    iList         = iHashTable.LookUp("List");
    iProg         = iHashTable.LookUp("Prog");

    iEndOfFile   ->IncreaseRefCount();
    iEndStatement->IncreaseRefCount();
    iProgOpen    ->IncreaseRefCount();
    iProgClose   ->IncreaseRefCount();
    iNth         ->IncreaseRefCount();
    iBracketOpen ->IncreaseRefCount();
    iBracketClose->IncreaseRefCount();
    iListOpen    ->IncreaseRefCount();
    iListClose   ->IncreaseRefCount();
    iComma       ->IncreaseRefCount();
    iList        ->IncreaseRefCount();
    iProg        ->IncreaseRefCount();
}

InfixParser::~InfixParser()
{
    iEndOfFile   ->DecreaseRefCount();
    iEndStatement->DecreaseRefCount();
    iProgOpen    ->DecreaseRefCount();
    iProgClose   ->DecreaseRefCount();
    iNth         ->DecreaseRefCount();
    iBracketOpen ->DecreaseRefCount();
    iBracketClose->DecreaseRefCount();
    iListOpen    ->DecreaseRefCount();
    iListClose   ->DecreaseRefCount();
    iComma       ->DecreaseRefCount();
    iList        ->DecreaseRefCount();
    iProg        ->DecreaseRefCount();
    }




void InfixParser::Parse(LispPtr& aResult )
{
    ParsedObject object(*this);
    object.Parse();
    aResult = object.iResult;
    return ;
}

void LispOperators::SetOperator(LispInt aPrecedence, LispStringPtr aString)
{
    LispInFixOperator op(aPrecedence);
    SetAssociation(op, aString);
}


void LispOperators::SetRightAssociative(LispStringPtr aString)
{
    LispInFixOperator* op = LookUp(aString);
    Check(op != NULL,KLispErrNotAnInFixOperator);
    op->SetRightAssociative();
}

void LispOperators::SetLeftPrecedence(LispStringPtr aString,LispInt aPrecedence)
{
    LispInFixOperator* op = LookUp(aString);
    Check(op != NULL,KLispErrNotAnInFixOperator);
    op->SetLeftPrecedence(aPrecedence);
}

void LispOperators::SetRightPrecedence(LispStringPtr aString,LispInt aPrecedence)
{
    LispInFixOperator* op = LookUp(aString);
    Check(op != NULL,KLispErrNotAnInFixOperator);
    op->SetRightPrecedence(aPrecedence);
}


void ParsedObject::ReadToken()
{
    // Get token.
    iLookAhead = iParser.iTokenizer.NextToken(iParser.iInput,
                                              iParser.iHashTable);
    if (iLookAhead->String()[0] == '\0')
        iEndOfFile=LispTrue;
}

void ParsedObject::MatchToken(LispStringPtr aToken)
{
    if (aToken != iLookAhead)
        iError=LispTrue;
    ReadToken();
}

void ParsedObject::Parse()
{
    ReadToken();
    if (iEndOfFile)
    {
        iResult.Set(LispAtom::New(iParser.iEndOfFile));
        return;
    }

    ReadExpression(KMaxPrecedence);  // least precedence

    if (iLookAhead != iParser.iEndStatement)
        iError = LispTrue;
    if (iError)
    {
        while ((*iLookAhead)[0] != '\0' && iLookAhead != iParser.iEndStatement)
        {
            ReadToken();
        }
    }

    if (iError)
    {
        iResult.Set(NULL);
    }
    Check(!iError,KLispErrInvalidExpression);
}


void ParsedObject::Combine(LispInt aNrArgsToCombine)
{
    LispPtr subList;
    subList.Set(LispSubList::New(iResult.Get()));
#ifdef DEBUG_MODE
    subList.Get()->SetFileAndLine(
                              iParser.iInput.Status().FileName(),
                              iParser.iInput.Status().LineNumber()
                             );
#endif

    LispIterator iter(iResult);
    LispInt i;
    for (i=0;i<aNrArgsToCombine;i++)
    {
        if (iter() == NULL)
        {
            iError = LispTrue;
            return;
        }
        iter.GoNext();
    }
    if (iter() == NULL)
    {
        iError = LispTrue;
        return;
    }
    subList.Get()->Next().Set(iter()->Next().Get());
    iter()->Next().Set(NULL);

    InternalReverseList(subList.Get()->SubList()->Get()->Next(),
                     subList.Get()->SubList()->Get()->Next());
    iResult = subList;
}

void ParsedObject::GetOtherSide(LispInt aNrArgsToCombine, LispInt depth)
{
    LispStringPtr theOperator = iLookAhead;
    MatchToken(iLookAhead);
    ReadExpression(depth);
    InsertAtom(theOperator);
    Combine(aNrArgsToCombine);
}

void ParsedObject::InsertAtom(LispStringPtr aString)
{
    LispPtr ptr;
    ptr.Set(LispAtom::New(aString));
#ifdef DEBUG_MODE
    ptr.Get()->SetFileAndLine(
                              iParser.iInput.Status().FileName(),
                              iParser.iInput.Status().LineNumber()
                             );
#endif
    ptr.Get()->Next().Set(iResult.Get());
    iResult.Set(ptr.Get());
}


void ParsedObject::ReadExpression(LispInt depth)
{
    ReadAtom();

    for(;;)
    {
        //Handle special case: a[b]. a is matched with lowest precedence!!
        if (iLookAhead == iParser.iProgOpen)
        {
            // Match opening bracket
            MatchToken(iLookAhead);
            // Read "index" argument
            ReadExpression(KMaxPrecedence);
            // Match closing bracket
            if (iLookAhead != iParser.iProgClose)
            {
                iError = LispTrue;
                return;
            }
            MatchToken(iLookAhead);
            // Build into Ntn(...)
            LispStringPtr theOperator = iParser.iNth;
            InsertAtom(theOperator);
            Combine(2);
        }
        else
        {
            LispInFixOperator* op = iParser.iInfixOperators.LookUp(iLookAhead);
            if (op == NULL)
                return;
            if (depth < op->iPrecedence)
                return;
            LispInt upper=op->iPrecedence;
            if (!op->iRightAssociative)
                upper--;
            GetOtherSide(2,upper);
        }
    }
}



void ParsedObject::ReadAtom()
{
    LispInFixOperator* op;
    // Parse prefix operators
    op = iParser.iPrefixOperators.LookUp(iLookAhead);
    if (op != NULL)
    {
        LispStringPtr theOperator = iLookAhead;
        MatchToken(iLookAhead);

        LispInt negativeNumber = 0;

        if (!StrCompare(theOperator->String(), "-"))
        {
            LispChar c = iLookAhead->String()[0];
            if (IsDigit(c) || c == '.')
                negativeNumber = 1;
        }
        if (negativeNumber)
        {
            LispString str(iLookAhead->String());
            {
                LispInt neg = 0;
                LispInt nr = iLookAhead->NrItems()-1;
                LispInt i;
                LispCharPtr p = iLookAhead->String();
                for (i=0;i<nr && !neg;i++)
                {
                    if (p[i] != '0' && p[i] != '.')
                        neg = 1;
                }
                if (neg)
                {
                    LispChar c = '-';
                    str.Insert(0,c,1);
                }
            }

            InsertAtom(iParser.iHashTable.LookUp(str.String()));
            MatchToken(iLookAhead);
        }
        else
        {
            ReadExpression(op->iPrecedence);
            InsertAtom(theOperator);
            Combine(1);
        }
    }
    // Else parse brackets
    else if (iLookAhead == iParser.iBracketOpen)
    {
        MatchToken(iLookAhead);
        ReadExpression(KMaxPrecedence);  // least precedence
        MatchToken(iParser.iBracketClose);
    }
    //Parse lists
    else if (iLookAhead == iParser.iListOpen)
    {
        LispInt nrargs=0;
        MatchToken(iLookAhead);
        while (iLookAhead != iParser.iListClose)
        {
            ReadExpression(KMaxPrecedence);  // least precedence
            nrargs++;

            if (iLookAhead == iParser.iComma)
            {
                MatchToken(iLookAhead);
            }
            else if (iLookAhead != iParser.iListClose)
            {
                iError = LispTrue;
                return;
            }
        }
        MatchToken(iLookAhead);
        LispStringPtr theOperator = iParser.iList;
        InsertAtom(theOperator);
        Combine(nrargs);

    }
    // Parse prog bodies
    else if (iLookAhead == iParser.iProgOpen)
    {
        LispInt nrargs=0;
        MatchToken(iLookAhead);
        while (iLookAhead != iParser.iProgClose)
        {
            ReadExpression(KMaxPrecedence);  // least precedence
            nrargs++;

            if (iLookAhead == iParser.iEndStatement)
            {
                MatchToken(iLookAhead);
            }
            else
            {
                iError = LispTrue;
                return;
            }
        }
        MatchToken(iLookAhead);
        LispStringPtr theOperator = iParser.iProg;
        InsertAtom(theOperator);
        Combine(nrargs);
    }
    // Else we have an atom.
    else
    {
        LispStringPtr theOperator = iLookAhead;
        MatchToken(iLookAhead);

        LispInt nrargs=-1;
        if (iLookAhead == iParser.iBracketOpen)
        {
            nrargs=0;
            MatchToken(iLookAhead);
            while (iLookAhead != iParser.iBracketClose)
            {
                ReadExpression(KMaxPrecedence);  // least precedence
                nrargs++;

                if (iLookAhead == iParser.iComma)
                {
                    MatchToken(iLookAhead);
                }
                else if (iLookAhead != iParser.iBracketClose)
                {
                    iError = LispTrue;
                    return;
                }
            }
            MatchToken(iLookAhead);

            op = iParser.iBodiedOperators.LookUp(theOperator);
            if (op)
            {
                ReadExpression(op->iPrecedence); // KMaxPrecedence
                nrargs++;
            }
        }
        InsertAtom(theOperator);
        if (nrargs>=0)
            Combine(nrargs);

    }

    // Parse postfix operators
    op = iParser.iPostfixOperators.LookUp(iLookAhead);
    if (op != NULL)
    {
        InsertAtom(iLookAhead);
        MatchToken(iLookAhead);
        Combine(1);
    }
}


void InfixPrinter::WriteToken(LispOutput& aOutput,LispCharPtr aString)
{
    if (IsAlNum(iPrevLastChar) && (IsAlNum(aString[0]) || aString[0]=='_'))
    {
        aOutput.Write(" ");
    }
    else if (IsSymbolic(iPrevLastChar) && IsSymbolic(aString[0]))
    {
        aOutput.Write(" ");
    }
    aOutput.Write(aString);
    iPrevLastChar = aString[PlatStrLen(aString)-1];
}

void InfixPrinter::Print(LispPtr& aExpression, LispOutput& aOutput)
{
    Print(aExpression, aOutput, KMaxPrecedence);
}

void InfixPrinter::Print(LispPtr& aExpression, LispOutput& aOutput,
                         LispInt iPrecedence)
{
    LISPASSERT(aExpression.Get() != NULL);

    LispStringPtr string = aExpression.Get()->String();
    if (string != NULL)
    {
        WriteToken(aOutput,string->String());
        return;
    }

    if (aExpression.Get()->Generic() != NULL)
    {
        //TODO display genericclass
        WriteToken(aOutput,aExpression.Get()->Generic()->TypeName());
        return;
    }

    LispPtr* subList = aExpression.Get()->SubList();
    Check(subList!=NULL, KLispErrUnprintableToken);
    if (subList->Get() == NULL)
    {
        WriteToken(aOutput,"( )");
    }
    else
    {
        LispInt length = InternalListLength(*subList);
        string = subList->Get()->String();
        LispInFixOperator* prefix  = iPrefixOperators.LookUp(string);
        LispInFixOperator* infix   = iInfixOperators.LookUp(string);
        LispInFixOperator* postfix = iPostfixOperators.LookUp(string);
        LispInFixOperator* bodied  = iBodiedOperators.LookUp(string);
        LispInFixOperator* op = NULL;

        if (length!=2)
        {
            prefix=NULL;
            postfix=NULL;
        }
        if (length!=3)
        {
            infix=NULL;
        }
        if (prefix  )  op=prefix;
        if (postfix )  op=postfix;
        if (infix   )  op=infix;

        if (op)
        {
            LispPtr* left  = NULL;
            LispPtr* right = NULL;

            if (prefix)
            {
                right = &subList->Get()->Next();
            }
            else if (infix)
            {
                left  = &subList->Get()->Next();
                right = &subList->Get()->Next().Get()->Next();
            }
            else if (postfix)
            {
                left = &subList->Get()->Next();
            }

            if (iPrecedence < op->iPrecedence){
                WriteToken(aOutput,"(");
            }
            else
            {
            //Vladimir?    aOutput.Write(" ");
            }
            if (left)
                Print(*left, aOutput,op->iLeftPrecedence);
            WriteToken(aOutput,string->String());
            if (right)
                Print(*right, aOutput,op->iRightPrecedence);
            if (iPrecedence < op->iPrecedence)
                WriteToken(aOutput,")");
        }
        else
        {
            LispIterator iter(subList->Get()->Next());
            if (*string == "List")
            {
                WriteToken(aOutput,"{");
                while (iter())
                {
                    Print(*iter.Ptr(), aOutput, KMaxPrecedence);
                    iter.GoNext();
                    if (iter())
                        WriteToken(aOutput,",");
                }
                WriteToken(aOutput,"}");
            }
            else if (*string == "Prog")
            {
                WriteToken(aOutput,"[");
                while (iter())
                {
                    Print(*iter.Ptr(), aOutput, KMaxPrecedence);
                    iter.GoNext();
                    WriteToken(aOutput,";");
                }
                WriteToken(aOutput,"]");
            }
            else if (*string == "Nth")
            {
                Print(*iter.Ptr(), aOutput, 0);
                iter.GoNext();
                WriteToken(aOutput,"[");
                Print(*iter.Ptr(), aOutput, KMaxPrecedence);
                WriteToken(aOutput,"]");
            }
            else
            {
                WriteToken(aOutput,string->String());
                WriteToken(aOutput,"(");

                LispIterator counter(*iter.Ptr());
                LispInt nr=0;

                while (counter())
                {
                    counter.GoNext();
                    nr++;
                }

                if (bodied)
                    nr--;
                while (nr--)
                {
                    Print(*iter.Ptr(), aOutput, KMaxPrecedence);

                    iter.GoNext();
                    if (nr)
                        WriteToken(aOutput,",");
                }
                WriteToken(aOutput,")");
                if (iter())
                    Print(*iter.Ptr(), aOutput, bodied->iPrecedence);
            }
        }
    }
}
