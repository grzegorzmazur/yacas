/** \file infixparser.h
 *  parsing and printing in the infix style.
 *
 */

#ifndef YACAS_INFIXPARSER_H
#define YACAS_INFIXPARSER_H

#include "yacasbase.h"
#include "lispparser.h"


#define KMaxPrecedence 60000

class LispInFixOperator : public YacasBase
{
public:
    inline LispInFixOperator(LispInt aPrecedence)
        : iPrecedence(aPrecedence),
        iLeftPrecedence(aPrecedence),
        iRightPrecedence(aPrecedence),
        iRightAssociative(0)
    {};
    inline void SetRightAssociative(void)
    {
        iRightAssociative = 1;
    }
    inline void SetLeftPrecedence(LispInt aPrecedence)
    {
        iLeftPrecedence = aPrecedence;
    }
    inline void SetRightPrecedence(LispInt aPrecedence)
    {
        iRightPrecedence = aPrecedence;
    }
public:
    LispInt iPrecedence;
    LispInt iLeftPrecedence;
    LispInt iRightPrecedence;
    LispInt iRightAssociative;
};

class LispOperators : public LispAssociatedHash<LispInFixOperator>
{
public:
    void SetOperator(LispInt aPrecedence,LispString * aString);
    void SetRightAssociative(LispString * aString);
    void SetLeftPrecedence(LispString * aString,LispInt aPrecedence);
    void SetRightPrecedence(LispString * aString,LispInt aPrecedence);
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
          iPrevLastChar(0),iCurrentEnvironment(NULL){}

    virtual void Print(LispPtr& aExpression, LispOutput& aOutput,
                       LispEnvironment& aEnvironment);
  virtual void RememberLastChar(LispChar aChar);
private:
    void Print(LispPtr& aExpression, LispOutput& aOutput,
               LispInt iPrecedence);
    void WriteToken(LispOutput& aOutput,const LispChar * aString);
private:
    LispOperators& iPrefixOperators;
    LispOperators& iInfixOperators;
    LispOperators& iPostfixOperators;
    LispOperators& iBodiedOperators;
    LispChar iPrevLastChar;
    LispEnvironment* iCurrentEnvironment;
};


#endif

