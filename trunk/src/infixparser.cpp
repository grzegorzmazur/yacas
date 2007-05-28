
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
   if (iLookAhead && iLookAhead->c_str())
   {
//    if (IsSymbolic(iLookAhead->c_str()[0]))
//       RaiseError("Error parsing expression, near token %s (maybe you forgot a space between two operators?)",iLookAhead->c_str());
      RaiseError("Error parsing expression, near token %s",iLookAhead->c_str());
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

void LispOperators::SetOperator(LispInt aPrecedence, LispString * aString)
{
    LispInFixOperator op(aPrecedence);
    SetAssociation(op, aString);
}


void LispOperators::SetRightAssociative(LispString * aString)
{
    LispInFixOperator* op = LookUp(aString);
    Check(op,KLispErrNotAnInFixOperator);
    op->SetRightAssociative();
}

void LispOperators::SetLeftPrecedence(LispString * aString,LispInt aPrecedence)
{
    LispInFixOperator* op = LookUp(aString);
    Check(op,KLispErrNotAnInFixOperator);
    op->SetLeftPrecedence(aPrecedence);
}

void LispOperators::SetRightPrecedence(LispString * aString,LispInt aPrecedence)
{
    LispInFixOperator* op = LookUp(aString);
    Check(op,KLispErrNotAnInFixOperator);
    op->SetRightPrecedence(aPrecedence);
}


void ParsedObject::ReadToken()
{
    // Get token.
    iLookAhead = iParser.iTokenizer.NextToken(iParser.iInput,
                                              iParser.iEnvironment.HashTable());
    if (iLookAhead->c_str()[0] == '\0')
        iEndOfFile=LispTrue;
}

void ParsedObject::MatchToken(LispString * aToken)
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
        iResult = (iParser.iEnvironment.iEndOfFile->Copy());
        return;
    }

    ReadExpression(KMaxPrecedence);  // least precedence

    if (iLookAhead != iParser.iEnvironment.iEndStatement->String())
    {
      RAISE_PARSE_ERROR; // iError = LispTrue;
    }
    if (iError)
    {
        while ((*iLookAhead)[0] != '\0' && iLookAhead != iParser.iEnvironment.iEndStatement->String())
        {
            ReadToken();
        }
    }

    if (iError)
    {
        iResult = (NULL);
    }
    Check(!iError,KLispErrInvalidExpression);
}


void ParsedObject::Combine(LispInt aNrArgsToCombine)
{
    LispPtr subList(LispSubList::New(iResult));
    DBG_( subList->SetFileAndLine(
    iParser.iInput.Status().FileName(),
    iParser.iInput.Status().LineNumber() ); )

  // TODO: woof -- such ugliness!

    LispIterator iter(iResult);
    for (LispInt i=0; i<aNrArgsToCombine; i++, ++iter)
    {
    if (!iter.getObj()) { RAISE_PARSE_ERROR; return; } // iError = LispTrue;
  }
  if (!iter.getObj()) { RAISE_PARSE_ERROR; return; } // iError = LispTrue;
    subList->Nixed() = (*++iter);
    *iter = (NULL);

  InternalReverseList((*subList->SubList())->Nixed(),  // TODO: woof
                     (*subList->SubList())->Nixed());
    iResult = subList;
}

void ParsedObject::GetOtherSide(LispInt aNrArgsToCombine, LispInt depth)
{
    LispString * theOperator = iLookAhead;
    MatchToken(iLookAhead);
    ReadExpression(depth);
    InsertAtom(theOperator);
    Combine(aNrArgsToCombine);
}

void ParsedObject::InsertAtom(LispString * aString)
{
    LispPtr ptr(LispAtom::New(iParser.iEnvironment,aString->c_str()));
    DBG_( ptr->SetFileAndLine(
    iParser.iInput.Status().FileName(),
    iParser.iInput.Status().LineNumber() ); )
    ptr->Nixed() = (iResult);
    iResult = (ptr);
}


void ParsedObject::ReadExpression(LispInt depth)
{
    ReadAtom();

    for(;;)
    {
        //Handle special case: a[b]. a is matched with lowest precedence!!
        if (iLookAhead == iParser.iEnvironment.iProgOpen->String())
        {
            // Match opening bracket
            MatchToken(iLookAhead);
            // Read "index" argument
            ReadExpression(KMaxPrecedence);
            // Match closing bracket
            if (iLookAhead != iParser.iEnvironment.iProgClose->String())
            {
                RaiseError("Expecting a ] close bracket for program block, but got %s instead",iLookAhead->c_str()); // RAISE_PARSE_ERROR; // iError = LispTrue;
                return;
            }
            MatchToken(iLookAhead);
            // Build into Ntn(...)
            LispString * theOperator = iParser.iEnvironment.iNth->String();
            InsertAtom(theOperator);
            Combine(2);
        }
        else
        {
            LispInFixOperator* op = iParser.iInfixOperators.LookUp(iLookAhead);
            if (!op)
            {
//printf("op [%s]\n",iLookAhead->c_str());
              if (IsSymbolic((*iLookAhead)[0]))
              {
                LispInt origlen = iLookAhead->Size()-1;
                LispInt len = origlen;
//printf("IsSymbolic, len=%d\n",len);

                while (len>1)
                {
                  len--;
                  LispString * lookUp =
                    iParser.iEnvironment.HashTable().LookUpCounted(iLookAhead->c_str(),len);

//printf("trunc %s\n",lookUp->c_str());
                  op = iParser.iInfixOperators.LookUp(lookUp);
//if (op) printf("FOUND\n");
                  if (op)
                  {

                    LispString * lookUpRight =
                      iParser.iEnvironment.HashTable().LookUpCounted(&(iLookAhead->c_str()[len]),origlen-len);

//printf("right: %s (%d)\n",lookUpRight->c_str(),origlen-len);

                    if (iParser.iPrefixOperators.LookUp(lookUpRight))
                    {
//printf("ACCEPT %s\n",lookUp->c_str());
                      iLookAhead = lookUp;
                      LispInput& input = iParser.iInput;
                      LispInt newPos = input.Position()-(origlen-len);
                      input.SetPosition(newPos);
//printf("Pushhback %s\n",&input.StartPtr()[input.Position()]);
                      break;
                    }
                    else op=NULL;
                  }
                }
                if (!op) return;
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
    if (op)
    {
        LispString * theOperator = iLookAhead;
        MatchToken(iLookAhead);
        {
            ReadExpression(op->iPrecedence);
            InsertAtom(theOperator);
            Combine(1);
        }
    }
    // Else parse brackets
    else if (iLookAhead == iParser.iEnvironment.iBracketOpen->String())
    {
        MatchToken(iLookAhead);
        ReadExpression(KMaxPrecedence);  // least precedence
        MatchToken(iParser.iEnvironment.iBracketClose->String());
    }
    //Parse lists
    else if (iLookAhead == iParser.iEnvironment.iListOpen->String())
    {
        LispInt nrargs=0;
        MatchToken(iLookAhead);
        while (iLookAhead != iParser.iEnvironment.iListClose->String())
        {
            ReadExpression(KMaxPrecedence);  // least precedence
            nrargs++;

            if (iLookAhead == iParser.iEnvironment.iComma->String())
            {
                MatchToken(iLookAhead);
            }
            else if (iLookAhead != iParser.iEnvironment.iListClose->String())
            {
                RaiseError("Expecting a } close bracket for a list, but got %s instead",iLookAhead->c_str()); // RAISE_PARSE_ERROR; // iError = LispTrue;
                return;
            }
        }
        MatchToken(iLookAhead);
        LispString * theOperator = iParser.iEnvironment.iList->String();
        InsertAtom(theOperator);
        Combine(nrargs);

    }
    // Parse prog bodies
    else if (iLookAhead == iParser.iEnvironment.iProgOpen->String())
    {
        LispInt nrargs=0;
        // For prog bodies at least, I'd prefer the line it points to to be the line of the opening bracket.
        // Maybe we should change this for all expressions: the line number referring to the first atom in
        // the expression in stead of the last.
        // Ayal
        DBG_( LispInt startLine = iParser.iInput.Status().LineNumber(); )

        MatchToken(iLookAhead);
        while (iLookAhead != iParser.iEnvironment.iProgClose->String())
        {
            ReadExpression(KMaxPrecedence);  // least precedence
            nrargs++;

            if (iLookAhead == iParser.iEnvironment.iEndStatement->String())
            {
                MatchToken(iLookAhead);
            }
            else
            {
                RaiseError("Expecting ; end of statement in program block, but got %s instead",iLookAhead->c_str()); // RAISE_PARSE_ERROR; // iError = LispTrue;
                return;
            }
        }
        MatchToken(iLookAhead);
        LispString * theOperator = iParser.iEnvironment.iProg->String();
        InsertAtom(theOperator);

        Combine(nrargs);
        // Set the line to the beginning of the prog
        DBG_( iResult->SetFileAndLine(
      iParser.iInput.Status().FileName(),
      startLine ); )
    }
    // Else we have an atom.
    else
    {
        LispString * theOperator = iLookAhead;
        MatchToken(iLookAhead);

        LispInt nrargs=-1;
        if (iLookAhead == iParser.iEnvironment.iBracketOpen->String())
        {
            nrargs=0;
            MatchToken(iLookAhead);
            while (iLookAhead != iParser.iEnvironment.iBracketClose->String())
            {
                ReadExpression(KMaxPrecedence);  // least precedence
                nrargs++;

                if (iLookAhead == iParser.iEnvironment.iComma->String())
                {
                    MatchToken(iLookAhead);
                }
                else if (iLookAhead != iParser.iEnvironment.iBracketClose->String())
                {
                    RaiseError("Expecting ) closing bracket for sub-expression, but got %s instead",iLookAhead->c_str()); // RAISE_PARSE_ERROR; // iError = LispTrue;
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

    while ((op = iParser.iPostfixOperators.LookUp(iLookAhead)))
    {
        InsertAtom(iLookAhead);
        MatchToken(iLookAhead);
        Combine(1);
    }
}


void InfixPrinter::WriteToken(LispOutput& aOutput,LispChar * aString)
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
    LISPASSERT(aExpression);

    LispString * string = aExpression->String();
    if (string)
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
        WriteToken(aOutput,string->c_str());
        if (bracket) WriteToken(aOutput,")");
        return;
    }

    if (aExpression->Generic())
    {
        //TODO display genericclass
        WriteToken(aOutput,aExpression->Generic()->TypeName());
        return;
    }

    LispPtr* subList = aExpression->SubList();
    Check(subList, KLispErrUnprintableToken);
    if (!subList)
    {
        WriteToken(aOutput,"( )");
    }
    else
    {
        LispInt length = InternalListLength(*subList);
        string = (*subList)->String();
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
                right = &(*subList)->Nixed();
            }
            else if (infix)
            {
                left  = &(*subList)->Nixed();
                right = &(*subList)->Nixed()->Nixed();
            }
            else if (postfix)
            {
                left = &(*subList)->Nixed();
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
            WriteToken(aOutput,string->c_str());
            if (right)
                Print(*right, aOutput,op->iRightPrecedence);
            if (iPrecedence < op->iPrecedence)
                WriteToken(aOutput,")");
        }
        else
        {
            LispIterator iter((*subList)->Nixed());
            if (string == iCurrentEnvironment->iList->String())
            {
                WriteToken(aOutput,"{");
                for (int ii = 0; iter.getObj(); ii++, ++iter)
        {
          if (ii) WriteToken(aOutput,",");
                    Print(*iter, aOutput, KMaxPrecedence);
                }
                WriteToken(aOutput,"}");
            }
            else if (string == iCurrentEnvironment->iProg->String())
            {
                WriteToken(aOutput,"[");
                while (iter.getObj())
                {
                    Print(*iter, aOutput, KMaxPrecedence);
                    ++iter;
                    WriteToken(aOutput,";");
                }
                WriteToken(aOutput,"]");
            }
            else if (string == iCurrentEnvironment->iNth->String())
            {
                Print(*iter, aOutput, 0);
                ++iter;
                WriteToken(aOutput,"[");
                Print(*iter, aOutput, KMaxPrecedence);
                WriteToken(aOutput,"]");
            }
            else
            {
                LispInt bracket = LispFalse;
                if (bodied)
                {
//printf("%d > %d\n",iPrecedence, bodied->iPrecedence);
                  if (iPrecedence < bodied->iPrecedence)
                    bracket = LispTrue;
                }
                if (bracket) WriteToken(aOutput,"(");
                if (string)
                {
                  WriteToken(aOutput,string->c_str());
                }
                else
                {
                  Print(*subList,aOutput,0);
                }
                WriteToken(aOutput,"(");

                LispIterator counter(*iter);
                LispInt nr=0;

                while (counter.getObj())
                {
                    ++counter;
                    nr++;
                }

                if (bodied)
                    nr--;
                while (nr--)
                {
                    Print(*iter, aOutput, KMaxPrecedence);
                    ++iter;
                    if (nr)
                        WriteToken(aOutput,",");
                }
                WriteToken(aOutput,")");
                if (iter.getObj())
                    Print(*iter, aOutput, bodied->iPrecedence);

                if (bracket) WriteToken(aOutput,")");
            }
        }
    }
}
