
#ifndef _customparser_h_
#define _customparser_h_

#include "tokenizer.h"



class CTokenizer : public LispTokenizer
{
    
public:
    /// NextToken returns a string representing the next token,
    /// or an empty list.
    virtual LispStringPtr NextToken(LispInput& aInput,
                                    LispHashTable& aHashTable);
    virtual ~CTokenizer(){}
private:
    LispString iToken; //Can be used as a token container.
};

#endif

