/** \file lispparser.h
 *  parsing and printing in the old-fashioned lisp style
 *
 */

#ifndef __lispparser_h__
#define __lispparser_h__

#include "yacasbase.h"
#include "lispobject.h"
#include "tokenizer.h"
#include "lispio.h"
#include "lisphash.h"
class LispParser : public YacasBase
{
public:
    LispParser(LispTokenizer& aTokenizer, LispInput& aInput,
               LispHashTable& aHashTable);
    virtual ~LispParser();
    virtual void Parse(LispPtr& aResult );
protected:
    void ParseList(LispPtr& aResult);
    void ParseAtom(LispPtr& aResult,LispStringPtr aToken);

public:
    LispTokenizer& iTokenizer;
    LispInput& iInput;
    LispHashTable& iHashTable;
};




class LispPrinter : public YacasBase
{
public:
    virtual ~LispPrinter();
    virtual void Print(LispPtr& aExpression, LispOutput& aOutput);
};


#endif
