/** \file tokenizer.h
 * definitions of input output classes that read and write from string.
 */


#ifndef __tokenizer_h__
#define __tokenizer_h__

#include "lispstring.h"
#include "lispio.h"
#include "lisphash.h"
class LispTokenizer
{
    
public:
    /// NextToken returns a string representing the next token,
    /// or an empty list.
    virtual LispStringPtr NextToken(LispInput& aInput,
                                    LispHashTable& aHashTable);
private:
    LispString iToken; //Can be used as a token container.
};

// utility functions
LispBoolean IsAlpha(LispChar c);
LispBoolean IsSymbolic(LispChar c);



#endif


