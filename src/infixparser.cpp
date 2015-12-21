
#include "yacas/yacasprivate.h"
#include "yacas/infixparser.h"
#include "yacas/lispatom.h"
#include "yacas/standard.h"
#include "yacas/lisperror.h"
#include "yacas/errors.h"

#include <cassert>
#include <string>

void ParsedObject::Fail()
{
    if (iLookAhead && !iLookAhead->empty())
        throw LispErrGeneric(std::string("Error parsing expression, near token ") + *iLookAhead);

    throw LispErrGeneric("Error parsing expression");
}

InfixParser::InfixParser(LispTokenizer& aTokenizer,
                         LispInput& aInput,
                         LispEnvironment& aEnvironment,
                         LispOperators& aPrefixOperators,
                         LispOperators& aInfixOperators,
                         LispOperators& aPostfixOperators,
                         LispOperators& aBodiedOperators) :
    LispParser(aTokenizer, aInput, aEnvironment),
    iPrefixOperators(aPrefixOperators),
    iInfixOperators(aInfixOperators),
    iPostfixOperators(aPostfixOperators),
    iBodiedOperators(aBodiedOperators)
{
}

void InfixParser::Parse(LispPtr& aResult)
{
    ParseCont(aResult);
}

void InfixParser::ParseCont(LispPtr& aResult)
{
    ParsedObject object(*this);
    object.Parse();
    aResult = object.iResult;
}

void LispOperators::SetOperator(LispInt aPrecedence, const LispString* aString)
{
    _map[aString] = LispInFixOperator(aPrecedence);
}

void LispOperators::SetRightAssociative(const LispString* aString)
{
    auto i = _map.find(aString);
    if (i == _map.end())
        throw LispErrNotAnInFixOperator();
    i->second.SetRightAssociative();
}

void LispOperators::SetLeftPrecedence(const LispString* aString, LispInt aPrecedence)
{
    auto i = _map.find(aString);
    if (i == _map.end())
        throw LispErrNotAnInFixOperator();
    i->second.SetLeftPrecedence(aPrecedence);
}

void LispOperators::SetRightPrecedence(const LispString* aString, LispInt aPrecedence)
{
    auto i = _map.find(aString);
    if (i == _map.end())
        throw LispErrNotAnInFixOperator();
    i->second.SetRightPrecedence(aPrecedence);
}

LispInFixOperator* LispOperators::LookUp(const LispString* aString)
{
    const auto i = _map.find(aString);
    if (i != _map.end())
        return &i->second;
    return nullptr;
}

void ParsedObject::ReadToken()
{
    // Get token.
    iLookAhead = iParser.iTokenizer.NextToken(iParser.iInput,
                                              iParser.iEnvironment.HashTable());
    if (iLookAhead->empty())
        iEndOfFile = true;
}

void ParsedObject::MatchToken(const LispString* aToken)
{
    if (aToken != iLookAhead)
        Fail();

    ReadToken();
}

void ParsedObject::Parse()
{
    ReadToken();
    if (iEndOfFile) {
        iResult = (iParser.iEnvironment.iEndOfFile->Copy());
        return;
    }

    ReadExpression(KMaxPrecedence); // least precedence

    if (iLookAhead != iParser.iEnvironment.iEndStatement->String())
        Fail();
}

void ParsedObject::Combine(LispInt aNrArgsToCombine)
{
    LispPtr subList(LispSubList::New(iResult));

    DBG_(subList->SetFileAndLine(
                                 iParser.iInput.Status().FileName(),
                                 iParser.iInput.Status().LineNumber()););

    // TODO: woof -- such ugliness!

    LispIterator iter(iResult);
    for (LispInt i = 0; i < aNrArgsToCombine; i++, ++iter)
        if (!iter.getObj())
            Fail();

    if (!iter.getObj())
        Fail();

    subList->Nixed() = *++iter;
    *iter = nullptr;

    InternalReverseList((*subList->SubList())->Nixed(), // TODO: woof
                        (*subList->SubList())->Nixed());
    iResult = subList;
}

void ParsedObject::GetOtherSide(LispInt aNrArgsToCombine, LispInt depth)
{
    const LispString* theOperator = iLookAhead;
    MatchToken(iLookAhead);
    ReadExpression(depth);
    InsertAtom(theOperator);
    Combine(aNrArgsToCombine);
}

void ParsedObject::InsertAtom(const LispString* aString)
{
    LispPtr ptr(LispAtom::New(iParser.iEnvironment, *aString));

    DBG_(ptr->SetFileAndLine(
                             iParser.iInput.Status().FileName(),
                             iParser.iInput.Status().LineNumber()););

    ptr->Nixed() = iResult;
    iResult = ptr;
}

void ParsedObject::ReadExpression(LispInt depth)
{
    ReadAtom();

    for (;;) {
        //Handle special case: a[b]. a is matched with lowest precedence!!
        if (iLookAhead == iParser.iEnvironment.iProgOpen->String()) {
            // Match opening bracket
            MatchToken(iLookAhead);
            // Read "index" argument
            ReadExpression(KMaxPrecedence);
            // Match closing bracket
            if (iLookAhead != iParser.iEnvironment.iProgClose->String())
                throw LispErrGeneric(std::string("Expecting a ] close bracket for program block, but got ") + *iLookAhead + std::string(" instead"));

            MatchToken(iLookAhead);
            // Build into Ntn(...)
            const LispString* theOperator = iParser.iEnvironment.iNth->String();
            InsertAtom(theOperator);
            Combine(2);
        } else {
            LispInFixOperator* op = iParser.iInfixOperators.LookUp(iLookAhead);
            if (!op) {
                if (IsSymbolic((*iLookAhead)[0])) {
                    LispInt origlen = iLookAhead->size();
                    LispInt len = origlen;

                    while (len > 1) {
                        len--;
                        const LispString* lookUp =
                                iParser.iEnvironment.HashTable().LookUp(iLookAhead->substr(0, len));

                        op = iParser.iInfixOperators.LookUp(lookUp);
                        if (op) {

                            const LispString* lookUpRight =
                                    iParser.iEnvironment.HashTable().LookUp(iLookAhead->substr(len, origlen - len));


                            if (iParser.iPrefixOperators.LookUp(lookUpRight)) {
                                iLookAhead = lookUp;
                                LispInput& input = iParser.iInput;
                                LispInt newPos = input.Position()-(origlen - len);
                                input.SetPosition(newPos);
                                break;
                            } else op = nullptr;
                        }
                    }
                    if (!op) return;
                } else {
                    return;
                }




                //              return;
            }
            if (depth < op->iPrecedence)
                return;
            LispInt upper = op->iPrecedence;
            if (!op->iRightAssociative)
                upper--;
            GetOtherSide(2, upper);
        }
    }
}

void ParsedObject::ReadAtom()
{
    LispInFixOperator* op;
    // Parse prefix operators
    op = iParser.iPrefixOperators.LookUp(iLookAhead);
    if (op) {
        const LispString* theOperator = iLookAhead;
        MatchToken(iLookAhead);
        {
            ReadExpression(op->iPrecedence);
            InsertAtom(theOperator);
            Combine(1);
        }
    }        // Else parse brackets
    else if (iLookAhead == iParser.iEnvironment.iBracketOpen->String()) {
        MatchToken(iLookAhead);
        ReadExpression(KMaxPrecedence); // least precedence
        MatchToken(iParser.iEnvironment.iBracketClose->String());
    }        //Parse lists
    else if (iLookAhead == iParser.iEnvironment.iListOpen->String()) {
        LispInt nrargs = 0;
        MatchToken(iLookAhead);
        while (iLookAhead != iParser.iEnvironment.iListClose->String()) {
            ReadExpression(KMaxPrecedence); // least precedence
            nrargs++;

            if (iLookAhead == iParser.iEnvironment.iComma->String()) {
                MatchToken(iLookAhead);
            } else if (iLookAhead != iParser.iEnvironment.iListClose->String()) {
                throw LispErrGeneric(std::string("Expecting a } close bracket for program block, but got ") + *iLookAhead + std::string(" instead"));
            }
        }
        MatchToken(iLookAhead);
        const LispString* theOperator = iParser.iEnvironment.iList->String();
        InsertAtom(theOperator);
        Combine(nrargs);

    }        // Parse prog bodies
    else if (iLookAhead == iParser.iEnvironment.iProgOpen->String()) {
        LispInt nrargs = 0;
        // For prog bodies at least, I'd prefer the line it points to to be the line of the opening bracket.
        // Maybe we should change this for all expressions: the line number referring to the first atom in
        // the expression in stead of the last.
        // Ayal
        DBG_(LispInt startLine = iParser.iInput.Status().LineNumber();)

                MatchToken(iLookAhead);
        while (iLookAhead != iParser.iEnvironment.iProgClose->String()) {
            ReadExpression(KMaxPrecedence); // least precedence
            nrargs++;

            if (iLookAhead == iParser.iEnvironment.iEndStatement->String()) {
                MatchToken(iLookAhead);
            } else {
                throw LispErrGeneric(std::string("Expecting ; end of statement in program block, but got ") + *iLookAhead + std::string(" instead"));
            }
        }
        MatchToken(iLookAhead);
        const LispString* theOperator = iParser.iEnvironment.iProg->String();
        InsertAtom(theOperator);

        Combine(nrargs);
        // Set the line to the beginning of the prog
        DBG_(iResult->SetFileAndLine(
                                     iParser.iInput.Status().FileName(),
                                     startLine);)
    }        // Else we have an atom.
    else {
        const LispString* theOperator = iLookAhead;
        MatchToken(iLookAhead);

        LispInt nrargs = -1;
        if (iLookAhead == iParser.iEnvironment.iBracketOpen->String()) {
            nrargs = 0;
            MatchToken(iLookAhead);
            while (iLookAhead != iParser.iEnvironment.iBracketClose->String()) {
                ReadExpression(KMaxPrecedence); // least precedence
                nrargs++;

                if (iLookAhead == iParser.iEnvironment.iComma->String()) {
                    MatchToken(iLookAhead);
                } else if (iLookAhead != iParser.iEnvironment.iBracketClose->String()) {
                    throw LispErrGeneric(std::string("Expecting a ) closing bracket for sub-expression, but got ") + *iLookAhead + std::string(" instead"));
                }
            }
            MatchToken(iLookAhead);

            op = iParser.iBodiedOperators.LookUp(theOperator);
            if (op) {
                ReadExpression(op->iPrecedence); // KMaxPrecedence
                nrargs++;
            }
        }
        InsertAtom(theOperator);
        if (nrargs >= 0)
            Combine(nrargs);

    }

    // Parse postfix operators

    while (iParser.iPostfixOperators.LookUp(iLookAhead)) {
        InsertAtom(iLookAhead);
        MatchToken(iLookAhead);
        Combine(1);
    }
}

void InfixPrinter::WriteToken(std::ostream& aOutput, const std::string& aString)
{
    if (IsAlNum(iPrevLastChar) && (IsAlNum(aString[0]) || aString[0] == '_'))
        aOutput.put(' ');
    else if (IsSymbolic(iPrevLastChar) && IsSymbolic(aString[0]))
        aOutput.put(' ');

    aOutput.write(aString.c_str(), aString.size());
    RememberLastChar(aString.back());
}

void InfixPrinter::RememberLastChar(LispChar aChar)
{
    iPrevLastChar = aChar;
}

void InfixPrinter::Print(
                         const LispPtr& aExpression,
                         std::ostream& aOutput,
                         LispEnvironment& aEnvironment)
{
    iCurrentEnvironment = &aEnvironment;
    Print(aExpression, aOutput, KMaxPrecedence);
}

void InfixPrinter::Print(
                         const LispPtr& aExpression,
                         std::ostream& aOutput,
                         LispInt iPrecedence)
{
    assert(aExpression);

    const LispString* string = aExpression->String();
    if (string) {
        LispInt bracket = 0;
        if (iPrecedence < KMaxPrecedence &&
                (*string)[0] == '-' &&
                (std::isdigit((*string)[1]) || (*string)[1] == '.')
                ) {
            bracket = 1;
        }
        if (bracket) WriteToken(aOutput, "(");
        WriteToken(aOutput, *string);
        if (bracket) WriteToken(aOutput, ")");
        return;
    }

    if (aExpression->Generic()) {
        //TODO display genericclass
        WriteToken(aOutput, aExpression->Generic()->TypeName());
        return;
    }

    LispPtr* subList = aExpression->SubList();
    if (!subList) {
        throw LispErrUnprintableToken();
    } else {
        LispInt length = InternalListLength(*subList);
        string = (*subList)->String();
        LispInFixOperator* prefix = iPrefixOperators.LookUp(string);
        LispInFixOperator* infix = iInfixOperators.LookUp(string);
        LispInFixOperator* postfix = iPostfixOperators.LookUp(string);
        LispInFixOperator* bodied = iBodiedOperators.LookUp(string);
        LispInFixOperator* op = nullptr;

        if (length != 2) {
            prefix = nullptr;
            postfix = nullptr;
        }
        if (length != 3) {
            infix = nullptr;
        }
        if (prefix) op = prefix;
        if (postfix) op = postfix;
        if (infix) op = infix;

        if (op) {
            LispPtr* left = nullptr;
            LispPtr* right = nullptr;

            if (prefix) {
                right = &(*subList)->Nixed();
            } else if (infix) {
                left = &(*subList)->Nixed();
                right = &(*subList)->Nixed()->Nixed();
            } else if (postfix) {
                left = &(*subList)->Nixed();
            }

            if (iPrecedence < op->iPrecedence) {
                WriteToken(aOutput, "(");
            } else {
                //Vladimir?    aOutput.Write(" ");
            }
            if (left)
                Print(*left, aOutput, op->iLeftPrecedence);
            WriteToken(aOutput, *string);
            if (right)
                Print(*right, aOutput, op->iRightPrecedence);
            if (iPrecedence < op->iPrecedence)
                WriteToken(aOutput, ")");
        } else {
            LispIterator iter((*subList)->Nixed());
            if (string == iCurrentEnvironment->iList->String()) {
                WriteToken(aOutput, "{");
                for (int ii = 0; iter.getObj(); ii++, ++iter) {
                    if (ii) WriteToken(aOutput, ",");
                    Print(*iter, aOutput, KMaxPrecedence);
                }
                WriteToken(aOutput, "}");
            } else if (string == iCurrentEnvironment->iProg->String()) {
                WriteToken(aOutput, "[");
                while (iter.getObj()) {
                    Print(*iter, aOutput, KMaxPrecedence);
                    ++iter;
                    WriteToken(aOutput, ";");
                }
                WriteToken(aOutput, "]");
            } else if (string == iCurrentEnvironment->iNth->String()) {
                Print(*iter, aOutput, 0);
                ++iter;
                WriteToken(aOutput, "[");
                Print(*iter, aOutput, KMaxPrecedence);
                WriteToken(aOutput, "]");
            } else {
                LispInt bracket = false;
                if (bodied) {
                    //printf("%d > %d\n",iPrecedence, bodied->iPrecedence);
                    if (iPrecedence < bodied->iPrecedence)
                        bracket = true;
                }
                if (bracket) WriteToken(aOutput, "(");
                if (string) {
                    WriteToken(aOutput, *string);
                } else {
                    Print(*subList, aOutput, 0);
                }
                WriteToken(aOutput, "(");

                LispIterator counter(*iter);
                LispInt nr = 0;

                while (counter.getObj()) {
                    ++counter;
                    nr++;
                }

                if (bodied)
                    nr--;
                while (nr--) {
                    Print(*iter, aOutput, KMaxPrecedence);
                    ++iter;
                    if (nr)
                        WriteToken(aOutput, ",");
                }
                WriteToken(aOutput, ")");
                if (iter.getObj()) {
                    assert(bodied);
                    Print(*iter, aOutput, bodied->iPrecedence);
                }

                if (bracket)
                    WriteToken(aOutput, ")");
            }
        }
    }
}
