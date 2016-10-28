
#include "yacas/infixparser.h"
#include "yacas/lispatom.h"
#include "yacas/standard.h"
#include "yacas/lisperror.h"
#include "yacas/errors.h"
#include "yacas/arrayclass.h"
#include "yacas/associationclass.h"

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

void ParsedObject::Combine(int aNrArgsToCombine)
{
    LispPtr subList(LispSubList::New(iResult));

    // TODO: woof -- such ugliness!

    LispIterator iter(iResult);
    for (int i = 0; i < aNrArgsToCombine; i++, ++iter)
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

void ParsedObject::GetOtherSide(int aNrArgsToCombine, int depth)
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

    ptr->Nixed() = iResult;
    iResult = ptr;
}

void ParsedObject::ReadExpression(int depth)
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
            LispOperators::const_iterator opi = iParser.iInfixOperators.find(iLookAhead);
            
            if (opi == iParser.iInfixOperators.end()) {
                if (!IsSymbolic((*iLookAhead)[0]))
                    return;
                
                const std::size_t origlen = iLookAhead->size();
                std::size_t len = origlen;

                while (len > 1) {
                    len -= 1;
                    const LispString* lookUp =
                            iParser.iEnvironment.HashTable().LookUp(iLookAhead->substr(0, len));

                    opi = iParser.iInfixOperators.find(lookUp);

                    if (opi != iParser.iInfixOperators.end()) {

                        const LispString* lookUpRight =
                                iParser.iEnvironment.HashTable().LookUp(iLookAhead->substr(len, origlen - len));

                        if (iParser.iPrefixOperators.find(lookUpRight) != iParser.iPrefixOperators.end()) {
                            iLookAhead = lookUp;
                            LispInput& input = iParser.iInput;
                            std::size_t newPos = input.Position() - (origlen - len);
                            input.SetPosition(newPos);
                            break;
                        }

                        opi = iParser.iInfixOperators.end();
                    }
                }

                if (opi == iParser.iInfixOperators.end())
                    return;
            }

            
            if (depth < opi->second.iPrecedence)
                return;
            int upper = opi->second.iPrecedence;
            if (!opi->second.iRightAssociative)
                upper--;
            GetOtherSide(2, upper);
        }
    }
}

void ParsedObject::ReadAtom()
{
    LispOperators::const_iterator opi =
            iParser.iPrefixOperators.find(iLookAhead);
    if (opi != iParser.iPrefixOperators.end()) {
        const LispString* theOperator = iLookAhead;
        MatchToken(iLookAhead);
        {
            ReadExpression(opi->second.iPrecedence);
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
        int nrargs = 0;
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
        int nrargs = 0;

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
    }        // Else we have an atom.
    else {
        const LispString* theOperator = iLookAhead;
        MatchToken(iLookAhead);

        int nrargs = -1;
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

            opi = iParser.iBodiedOperators.find(theOperator);
            if (opi != iParser.iBodiedOperators.end()) {
                ReadExpression(opi->second.iPrecedence); // KMaxPrecedence
                nrargs++;
            }
        }
        InsertAtom(theOperator);
        if (nrargs >= 0)
            Combine(nrargs);

    }

    // Parse postfix operators

    while (iParser.iPostfixOperators.find(iLookAhead) != iParser.iPostfixOperators.end()) {
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

void InfixPrinter::RememberLastChar(char aChar)
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
                         int iPrecedence)
{
    assert(aExpression);

    const LispString* string = aExpression->String();
    if (string) {
        int bracket = 0;
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

    if (const GenericClass* g = aExpression->Generic()) {
        if (const AssociationClass* a = dynamic_cast<const AssociationClass*>(g)) {
            WriteToken(aOutput, "Association");
            WriteToken(aOutput, "(");
            Print(a->ToList(), aOutput, KMaxPrecedence);
            WriteToken(aOutput, ")");
        } else if (const ArrayClass* a = dynamic_cast<const ArrayClass*>(g)) {
            WriteToken(aOutput, "Array");
            WriteToken(aOutput, "(");
            WriteToken(aOutput, "{");
            const std::size_t n = a->Size();
            for (std::size_t i = 1; i <= n; ++i) {
                Print(LispPtr(a->GetElement(i)), aOutput, KMaxPrecedence);
                if (i != n)
                    WriteToken(aOutput, ",");
            }
            WriteToken(aOutput, "}");
            WriteToken(aOutput, ")");
        } else {
            WriteToken(aOutput, g->TypeName());
        }
        return;
    }

    LispPtr* subList = aExpression->SubList();
    if (!subList) {
        throw LispErrUnprintableToken();
    } else {
        const std::size_t length = InternalListLength(*subList);
        string = (*subList)->String();

        const LispOperators::const_iterator prefix =
                length != 2
                ? iPrefixOperators.end() 
                : iPrefixOperators.find(string);
        
        const LispOperators::const_iterator infix =
                length != 3
                ? iInfixOperators.end()
                : iInfixOperators.find(string);
        
        const LispOperators::const_iterator postfix =
                length != 2
                ? iPostfixOperators.end()
                : iPostfixOperators.find(string);
        
        const LispOperators::const_iterator bodied =
                iBodiedOperators.find(string);
        
        const LispInFixOperator* op = nullptr;

        if (prefix != iPrefixOperators.end())
            op = &prefix->second;
        
        if (postfix != iPostfixOperators.end())
            op = &postfix->second;
        
        if (infix != iInfixOperators.end())
            op = &infix->second;

        if (op) {
            LispPtr* left = nullptr;
            LispPtr* right = nullptr;

            if (prefix != iPrefixOperators.end()) {
                right = &(*subList)->Nixed();
            } else if (infix != iInfixOperators.end()) {
                left = &(*subList)->Nixed();
                right = &(*subList)->Nixed()->Nixed();
            } else if (postfix  != iPostfixOperators.end()) {
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
                int bracket = false;
                if (bodied != iBodiedOperators.end()) {
                    //printf("%d > %d\n",iPrecedence, bodied->iPrecedence);
                    if (iPrecedence < bodied->second.iPrecedence)
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
                int nr = 0;

                while (counter.getObj()) {
                    ++counter;
                    nr++;
                }

                if (bodied != iBodiedOperators.end())
                    nr--;
                while (nr--) {
                    Print(*iter, aOutput, KMaxPrecedence);
                    ++iter;
                    if (nr)
                        WriteToken(aOutput, ",");
                }
                WriteToken(aOutput, ")");
                if (iter.getObj()) {
                    assert(bodied != iBodiedOperators.end());
                    Print(*iter, aOutput, bodied->second.iPrecedence);
                }

                if (bracket)
                    WriteToken(aOutput, ")");
            }
        }
    }
}
