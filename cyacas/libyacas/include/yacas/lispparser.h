/** \file lispparser.h
 *  parsing and printing in the old-fashioned lisp style
 *
 */

#ifndef YACAS_LISPPARSER_H
#define YACAS_LISPPARSER_H

#include "lispobject.h"
#include "tokenizer.h"
#include "lispio.h"
#include "evalfunc.h"

#include <ostream>

class LispParser {
public:
    LispParser(LispTokenizer& aTokenizer, LispInput& aInput,
               LispEnvironment& aEnvironment);

    virtual void Parse(LispPtr& aResult );
protected:
    void ParseList(LispPtr& aResult);
    void ParseAtom(LispPtr& aResult, const LispString* aToken);

public:
    LispTokenizer& iTokenizer;
    LispInput& iInput;
    LispEnvironment& iEnvironment;
    int iListed;
};

class LispPrinter {
public:
    virtual void Print(
        const LispPtr& aExpression,
        std::ostream& aOutput,
        LispEnvironment& aEnvironment);

    virtual void RememberLastChar(char aChar);

private:
    void PrintExpression(
        const LispPtr& aExpression,
        std::ostream& aOutput,
        LispEnvironment& aEnvironment,
        int aDepth=0);

    void Indent(std::ostream& aOutput, int aDepth);
};


#endif
