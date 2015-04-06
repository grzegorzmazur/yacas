/** \file tokenizer.h
 * definitions of input output classes that read and write from string.
 */


#ifndef YACAS_TOKENIZER_H
#define YACAS_TOKENIZER_H

#include "yacasbase.h"
#include "lispstring.h"
#include "lispio.h"
#include "lisphash.h"

#include <cctype>

class LispTokenizer : public YacasBase
{
public:
  LispTokenizer() : iToken() {}
  /// NextToken returns a string representing the next token,
  /// or an empty list.
  virtual const LispString* NextToken(LispInput& aInput,
                                      LispHashTable& aHashTable);
  virtual ~LispTokenizer(){}
protected:
  LispString iToken; //Can be used as a token container.
};

// utility functions
inline
bool IsAlpha(LispChar c)
{
    return std::isalpha(c) || c == '\'';
}

inline
bool IsAlNum(LispChar c)
{
  return IsAlpha(c) || std::isdigit(c);
}

bool IsSymbolic(LispChar c);

#endif


