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

#ifdef YACAS_NO_CONSTEXPR
const LispInt KMaxPrecedence = 60000;
#else
constexpr LispInt KMaxPrecedence = 60000;
#endif

class LispInFixOperator {
public:
    explicit
#ifndef YACAS_NO_CONSTEXPR
    constexpr
#endif
    LispInFixOperator(LispInt aPrecedence = KMaxPrecedence):
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
    void SetOperator(LispInt aPrecedence, const LispString* aString);
    void SetRightAssociative(const LispString* aString);
    void SetLeftPrecedence(const LispString* aString, LispInt aPrecedence);
    void SetRightPrecedence(const LispString* aString, LispInt aPrecedence);
    LispInFixOperator* LookUp(const LispString* aString);

private:
    std::unordered_map<const LispStringSmartPtr, LispInFixOperator, std::hash<const LispString*> > _map;
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
    void MatchToken(const LispString * aToken);
    void ReadExpression(LispInt depth);
    void ReadAtom();

private:
    void GetOtherSide(LispInt aNrArgsToCombine, LispInt depth);
    void Combine(LispInt aNrArgsToCombine);
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

    void WriteToken(std::ostream& aOutput, const std::string&  aString);

private:
    LispOperators& iPrefixOperators;
    LispOperators& iInfixOperators;
    LispOperators& iPostfixOperators;
    LispOperators& iBodiedOperators;
    LispChar iPrevLastChar;
    LispEnvironment* iCurrentEnvironment;
};


#endif

