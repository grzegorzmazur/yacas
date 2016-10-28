/** \file infixparser.h
 *  parsing and printing in the infix style.
 *
 */

#ifndef YACAS_INFIXPARSER_H
#define YACAS_INFIXPARSER_H

#include "lispparser.h"
#include "lispoperator.h"

#include <ostream>
#include <unordered_map>

class InfixParser final: public LispParser
{
public:
    InfixParser(LispTokenizer& aTokenizer,
                LispInput& aInput,
                LispEnvironment& aEnvironment,
                LispOperators& aPrefixOperators,
                LispOperators& aInfixOperators,
                LispOperators& aPostfixOperators,
                LispOperators& aBodiedOperators);

    void Parse(LispPtr& aResult) override;

public:
    LispOperators& iPrefixOperators;
    LispOperators& iInfixOperators;
    LispOperators& iPostfixOperators;
    LispOperators& iBodiedOperators;

private:
    void ParseCont(LispPtr& aResult);
};

class ParsedObject {
public:
    ParsedObject(InfixParser& aParser):
        iParser(aParser),
        iEndOfFile(false),
        iLookAhead(0),
        iResult(0)
    {
    }

    void Parse();

private:
    void ReadToken();
    void MatchToken(const LispString * aToken);
    void ReadExpression(int depth);
    void ReadAtom();

private:
    void GetOtherSide(int aNrArgsToCombine, int depth);
    void Combine(int aNrArgsToCombine);
    void InsertAtom(const LispString* aString);

private:
    void Fail(); // called when parsing fails, raising an exception

private:
    InfixParser& iParser;

private:
    bool iEndOfFile;
    const LispString* iLookAhead;

public:
    LispPtr iResult;
};


class InfixPrinter final: public LispPrinter
{
public:
    InfixPrinter(LispOperators& aPrefixOperators,
                 LispOperators& aInfixOperators,
                 LispOperators& aPostfixOperators,
                 LispOperators& aBodiedOperators)
        : iPrefixOperators(aPrefixOperators),
          iInfixOperators(aInfixOperators),
          iPostfixOperators(aPostfixOperators),
          iBodiedOperators(aBodiedOperators),
          iPrevLastChar(0),iCurrentEnvironment(nullptr){}

    void Print(
        const LispPtr& aExpression,
        std::ostream& aOutput,
        LispEnvironment& aEnvironment) override;

    void RememberLastChar(char aChar) override;

private:
    void Print(
        const LispPtr& aExpression,
        std::ostream& aOutput,
        int iPrecedence);

    void WriteToken(std::ostream& aOutput, const std::string&  aString);

private:
    LispOperators& iPrefixOperators;
    LispOperators& iInfixOperators;
    LispOperators& iPostfixOperators;
    LispOperators& iBodiedOperators;
    char iPrevLastChar;
    LispEnvironment* iCurrentEnvironment;
};


#endif

