/** \file infixparser.h
 *  parsing and printing in the infix style.
 *
 */

#ifndef YACAS_INFIXPARSER_H
#define YACAS_INFIXPARSER_H

#include "yacasbase.h"
#include "lispparser.h"

#include <ostream>
#include <unordered_map>

constexpr LispInt KMaxPrecedence = 60000;

class LispInFixOperator {
public:
    explicit constexpr LispInFixOperator(LispInt aPrecedence = KMaxPrecedence):
        iPrecedence(aPrecedence),
        iLeftPrecedence(aPrecedence),
        iRightPrecedence(aPrecedence),
        iRightAssociative(false)
    {}

    void SetRightAssociative()
    {
        iRightAssociative = true;
    }

    void SetLeftPrecedence(LispInt aPrecedence)
    {
        iLeftPrecedence = aPrecedence;
    }

    void SetRightPrecedence(LispInt aPrecedence)
    {
        iRightPrecedence = aPrecedence;
    }

    LispInt iPrecedence;
    LispInt iLeftPrecedence;
    LispInt iRightPrecedence;
    bool iRightAssociative;
};

class LispOperators {
public:
    void SetOperator(LispInt aPrecedence,LispString* aString);
    void SetRightAssociative(LispString* aString);
    void SetLeftPrecedence(LispString* aString, LispInt aPrecedence);
    void SetRightPrecedence(LispString* aString, LispInt aPrecedence);
    LispInFixOperator* LookUp(LispString* aString);

private:
    std::unordered_map<LispStringSmartPtr, LispInFixOperator, std::hash<LispString*> > _map;
};

class InfixParser : public LispParser
{
public:
    InfixParser(LispTokenizer& aTokenizer,
                LispInput& aInput,
                LispEnvironment& aEnvironment,
                LispOperators& aPrefixOperators,
                LispOperators& aInfixOperators,
                LispOperators& aPostfixOperators,
                LispOperators& aBodiedOperators);

    virtual void Parse(LispPtr& aResult);

public:
    LispOperators& iPrefixOperators;
    LispOperators& iInfixOperators;
    LispOperators& iPostfixOperators;
    LispOperators& iBodiedOperators;

private:
    void ParseCont(LispPtr& aResult);
};

class ParsedObject : public YacasBase
{
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
    void MatchToken(LispString * aToken);
    void ReadExpression(LispInt depth);
    void ReadAtom();

private:
    void GetOtherSide(LispInt aNrArgsToCombine, LispInt depth);
    void Combine(LispInt aNrArgsToCombine);
    void InsertAtom(LispString * aString);

private:
    void Fail(); // called when parsing fails, raising an exception

private:
    InfixParser& iParser;

private:
    bool iEndOfFile;
    LispString * iLookAhead;

public:
    LispPtr iResult;
};


class InfixPrinter : public LispPrinter
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

    virtual void Print(
        const LispPtr& aExpression,
        std::ostream& aOutput,
        LispEnvironment& aEnvironment);

    virtual void RememberLastChar(LispChar aChar);

private:
    void Print(
        const LispPtr& aExpression,
        std::ostream& aOutput,
        LispInt iPrecedence);

    void WriteToken(std::ostream& aOutput, const LispChar* aString);

private:
    LispOperators& iPrefixOperators;
    LispOperators& iInfixOperators;
    LispOperators& iPostfixOperators;
    LispOperators& iBodiedOperators;
    LispChar iPrevLastChar;
    LispEnvironment* iCurrentEnvironment;
};


#endif

