
#include "yacasprivate.h"
#include "lispassert.h"
#include "infixparser.h"
#include "lispatom.h"
#include "standard.h"
#include "lisperror.h"
#include "errors.h"


//#define RAISE_PARSE_ERROR { iError = LispTrue; RaiseError("Error parsing expression, near token %s",iLookAhead->String()); }


void ParsedObject::Fail()
{
  iError = LispTrue; 
  if (iLookAhead)
    if (iLookAhead->String())
    {
      if (IsSymbolic(iLookAhead->String()[0]))
        RaiseError("Error parsing expression, near token %s (maybe you forgot a space between two operators?)",iLookAhead->String()); 
      RaiseError("Error parsing expression, near token %s",iLookAhead->String()); 
    }
  RaiseError("Error parsing expression"); 
}
#define RAISE_PARSE_ERROR Fail() 

InfixParser::InfixParser(LispTokenizer& aTokenizer, LispInput& aInput,
                         LispEnvironment& aEnvironment,
                         LispOperators& aPrefixOperators,
                         LispOperators& aInfixOperators,
                         LispOperators& aPostfixOperators,
                         LispOperators& aBodiedOperators)
    : LispParser( aTokenizer,  aInput,aEnvironment),
    iPrefixOperators(aPrefixOperators),
    iInfixOperators(aInfixOperators),
    iPostfixOperators(aPostfixOperators),
    iBodiedOperators(aBodiedOperators)
{
}

InfixParser::~InfixParser()
{
}




void InfixParser::Parse(LispPtr& aResult )
{
//    iEnvironment = &aEnvironment;
    ParseCont(aResult);
}
void InfixParser::ParseCont(LispPtr& aResult)
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
                                              iParser.iEnvironment.HashTable());
    if (iLookAhead->String()[0] == '\0')
        iEndOfFile=LispTrue;
}

void ParsedObject::MatchToken(LispStringPtr aToken)
{
    if (aToken != iLookAhead)
          RAISE_PARSE_ERROR; // iError=LispTrue;
    ReadToken();
}

void ParsedObject::Parse()
{
    ReadToken();
    if (iEndOfFile)
    {
        iResult.Set(LispAtom::New(iParser.iEnvironment,iParser.iEnvironment.iEndOfFile));
        return;
    }

    ReadExpression(KMaxPrecedence);  // least precedence

    if (iLookAhead != iParser.iEnvironment.iEndStatement)
    {
      RAISE_PARSE_ERROR; // iError = LispTrue;
    }
    if (iError)
    {
        while ((*iLookAhead)[0] != '\0' && iLookAhead != iParser.iEnvironment.iEndStatement)
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
#ifdef YACAS_DEBUG
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
            RAISE_PARSE_ERROR; // iError = LispTrue;
            return;
        }
        iter.GoNext();
    }
    if (iter() == NULL)
    {
        RAISE_PARSE_ERROR; // iError = LispTrue;
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
    ptr.Set(LispAtom::New(iParser.iEnvironment,aString));
#ifdef YACAS_DEBUG
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
        if (iLookAhead == iParser.iEnvironment.iProgOpen)
        {
            // Match opening bracket
            MatchToken(iLookAhead);
            // Read "index" argument
            ReadExpression(KMaxPrecedence);
            // Match closing bracket
            if (iLookAhead != iParser.iEnvironment.iProgClose)
            {
                RaiseError("Expecting a ] close bracket for program block, but got %s instead",iLookAhead->String()); // RAISE_PARSE_ERROR; // iError = LispTrue;
                return;
            }
            MatchToken(iLookAhead);
            // Build into Ntn(...)
            LispStringPtr theOperator = iParser.iEnvironment.iNth;
            InsertAtom(theOperator);
            Combine(2);
        }
        else
        {
            LispInFixOperator* op = iParser.iInfixOperators.LookUp(iLookAhead);
            if (op == NULL)
            {
//printf("op [%s]\n",iLookAhead->String());
              if (IsSymbolic((*iLookAhead)[0]))
              {
                LispInt len = iLookAhead->NrItems()-1;
//printf("IsSymbolic, len=%d\n",len);

                while (len>1)
                {
                  len--;
                    LispStringPtr lookUp =
          iParser.iEnvironment.HashTable().LookUpCounted(iLookAhead->String(),len);

//printf("trunc %s\n",lookUp->String());
                    if (lookUp)
                    {
                      op = iParser.iInfixOperators.LookUp(lookUp);
//if (op) printf("FOUND\n");
                      if (op)
                      {
                        iLookAhead = lookUp;
                        LispInput& input = iParser.iInput;
                        LispInt newPos = input.Position()-(iLookAhead->NrItems()-len);
                        input.SetPosition(newPos);
//printf("Pushhback %s\n",&input.StartPtr()[input.Position()]);
                        break;
                      }
                    }
                }
                if (op == NULL) return;
              }
              else
              {
                return;
              }




//              return;
            }
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
        {
            ReadExpression(op->iPrecedence);
            InsertAtom(theOperator);
            Combine(1);
        }
    }
    // Else parse brackets
    else if (iLookAhead == iParser.iEnvironment.iBracketOpen)
    {
        MatchToken(iLookAhead);
        ReadExpression(KMaxPrecedence);  // least precedence
        MatchToken(iParser.iEnvironment.iBracketClose);
    }
    //Parse lists
    else if (iLookAhead == iParser.iEnvironment.iListOpen)
    {
        LispInt nrargs=0;
        MatchToken(iLookAhead);
        while (iLookAhead != iParser.iEnvironment.iListClose)
        {
            ReadExpression(KMaxPrecedence);  // least precedence
            nrargs++;

            if (iLookAhead == iParser.iEnvironment.iComma)
            {
                MatchToken(iLookAhead);
            }
            else if (iLookAhead != iParser.iEnvironment.iListClose)
            {
                RaiseError("Expecting a } close bracket for a list, but got %s instead",iLookAhead->String()); // RAISE_PARSE_ERROR; // iError = LispTrue;
                return;
            }
        }
        MatchToken(iLookAhead);
        LispStringPtr theOperator = iParser.iEnvironment.iList;
        InsertAtom(theOperator);
        Combine(nrargs);

    }
    // Parse prog bodies
    else if (iLookAhead == iParser.iEnvironment.iProgOpen)
    {
        LispInt nrargs=0;
        MatchToken(iLookAhead);
        while (iLookAhead != iParser.iEnvironment.iProgClose)
        {
            ReadExpression(KMaxPrecedence);  // least precedence
            nrargs++;

            if (iLookAhead == iParser.iEnvironment.iEndStatement)
            {
                MatchToken(iLookAhead);
            }
            else
            {
                RaiseError("Expecting ; end of statement in program block, but got %s instead",iLookAhead->String()); // RAISE_PARSE_ERROR; // iError = LispTrue;
                return;
            }
        }
        MatchToken(iLookAhead);
        LispStringPtr theOperator = iParser.iEnvironment.iProg;
        InsertAtom(theOperator);
        Combine(nrargs);
    }
    // Else we have an atom.
    else
    {
        LispStringPtr theOperator = iLookAhead;
        MatchToken(iLookAhead);

        LispInt nrargs=-1;
        if (iLookAhead == iParser.iEnvironment.iBracketOpen)
        {
            nrargs=0;
            MatchToken(iLookAhead);
            while (iLookAhead != iParser.iEnvironment.iBracketClose)
            {
                ReadExpression(KMaxPrecedence);  // least precedence
                nrargs++;

                if (iLookAhead == iParser.iEnvironment.iComma)
                {
                    MatchToken(iLookAhead);
                }
                else if (iLookAhead != iParser.iEnvironment.iBracketClose)
                {
                    RaiseError("Expecting ) closing bracket for sub-expression, but got %s instead",iLookAhead->String()); // RAISE_PARSE_ERROR; // iError = LispTrue;
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

    while ((op = iParser.iPostfixOperators.LookUp(iLookAhead)) != NULL)
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
	RememberLastChar(aString[PlatStrLen(aString)-1]);
}

void InfixPrinter::RememberLastChar(LispChar aChar)
{
	iPrevLastChar = aChar;
}

void InfixPrinter::Print(LispPtr& aExpression, LispOutput& aOutput,
                       LispEnvironment& aEnvironment)
{
    iCurrentEnvironment = &aEnvironment;
    Print(aExpression, aOutput, KMaxPrecedence);
}

void InfixPrinter::Print(LispPtr& aExpression, LispOutput& aOutput,
                         LispInt iPrecedence)
{
    LISPASSERT(aExpression.Get() != NULL);

    LispStringPtr string = aExpression.Get()->String();
    if (string != NULL)
    {
        LispInt bracket=0;
        if (iPrecedence<KMaxPrecedence &&
            (*string)[0] == '-' &&
            (IsDigit((*string)[1]) || (*string)[1] == '.')
           )
        {
            bracket=1;
        }
        if (bracket) WriteToken(aOutput,"(");
        WriteToken(aOutput,string->String());
        if (bracket) WriteToken(aOutput,")");
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
            if (string == iCurrentEnvironment->iList)
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
            else if (string == iCurrentEnvironment->iProg)
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
            else if (string == iCurrentEnvironment->iNth)
            {
                Print(*iter.Ptr(), aOutput, 0);
                iter.GoNext();
                WriteToken(aOutput,"[");
                Print(*iter.Ptr(), aOutput, KMaxPrecedence);
                WriteToken(aOutput,"]");
            }
            else
            {
                if (string != NULL)
                {
                  WriteToken(aOutput,string->String());
                }
                else
                {
                  Print(*subList,aOutput,0);
                }
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
