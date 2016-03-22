
#ifndef _xmltokenizer_h_
#define _xmltokenizer_h_

#include "tokenizer.h"

class XmlTokenizer final: public LispTokenizer
{
public:
  XmlTokenizer() {}
  /// NextToken returns a string representing the next token,
  /// or an empty list.
  const LispString* NextToken(LispInput& aInput,
                              LispHashTable& aHashTable) override;
};

#endif

