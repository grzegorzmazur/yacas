/** \file infixparser.h
 *  parsing and printing in the infix style.
 *
 */


#ifndef __infixparser_h__
#define __infixparser_h__

#include "lispparser.h"


#define KMaxPrecedence 60000

class LispInFixOperator
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
    void SetOperator(LispInt aPrecedence,LispStringPtr aString);
    void SetRightAssociative(LispStringPtr aString);
    void SetLeftPrecedence(LispStringPtr aString,LispInt aPrecedence);
    void SetRightPrecedence(LispStringPtr aString,LispInt aPrecedence);
};

class InfixParser : public LispParser
{
public:
    InfixParser(LispTokenizer& aTokenizer, LispInput& aInput,
                LispHashTable& aHashTable,
                LispOperators& aPrefixOperators,
                LispOperators& aInfixOperators,
                LispOperators& aPostfixOperators,
                LispOperators& aBodiedOperators);
    ~InfixParser();
    
    virtual void Parse(LispPtr& aResult );
public:
    LispOperators& iPrefixOperators;
    LispOperators& iInfixOperators;
    LispOperators& iPostfixOperators;
    LispOperators& iBodiedOperators;

    LispStringPtr iEndOfFile;
    LispStringPtr iEndStatement;
    LispStringPtr iProgOpen;
    LispStringPtr iProgClose;
    LispStringPtr iNth;
    LispStringPtr iBracketOpen;
    LispStringPtr iBracketClose;
    LispStringPtr iListOpen;
    LispStringPtr iListClose;
    LispStringPtr iComma;
    LispStringPtr iList;
    LispStringPtr iProg;
};

class ParsedObject
{
public:
    ParsedObject(InfixParser& aParser)
        : iParser(aParser),
          iError(LispFalse),
          iEndOfFile(LispFalse),
          iLookAhead(NULL)  {};
    void Parse();
private:
    void ReadToken();
    void MatchToken(LispStringPtr aToken);
    void ReadExpression(LispInt depth);
    void ReadAtom();
private:
    void GetOtherSide(LispInt aNrArgsToCombine, LispInt depth);
    void Combine(LispInt aNrArgsToCombine);
    void InsertAtom(LispStringPtr aString);
private:
    InfixParser& iParser;
private:
    LispBoolean iError;
    LispBoolean iEndOfFile;
    LispStringPtr iLookAhead;
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
          iPrevLastChar(0){}

    virtual void Print(LispPtr& aExpression, LispOutput& aOutput);
    void Print(LispPtr& aExpression, LispOutput& aOutput,
               LispInt iPrecedence);
private:
    void WriteToken(LispOutput& aOutput,LispCharPtr aString);
private:
    
    LispOperators& iPrefixOperators;
    LispOperators& iInfixOperators;
    LispOperators& iPostfixOperators;
    LispOperators& iBodiedOperators;
    LispChar iPrevLastChar;
};


#endif

