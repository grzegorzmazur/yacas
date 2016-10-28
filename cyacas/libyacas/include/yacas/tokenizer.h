/** \file tokenizer.h
 * definitions of input output classes that read and write from string.
 */


#ifndef YACAS_TOKENIZER_H
#define YACAS_TOKENIZER_H

#include "lispstring.h"
#include "lispio.h"
#include "lisphash.h"

#include <cctype>
#include <cstdint>

class LispTokenizer {
public:
  LispTokenizer() : iToken() {}
  virtual ~LispTokenizer() = default;

  /// NextToken returns a string representing the next token,
  /// or an empty list.
  virtual const LispString* NextToken(LispInput& aInput,
                                      LispHashTable& aHashTable);
protected:
  LispString iToken; //Can be used as a token container.
};

// utility functions
#ifdef YACAS_UINT32_T_IN_GLOBAL_NAMESPACE
    bool IsAlpha(uint32_t c);
    bool IsAlNum(uint32_t c);
#else
    bool IsAlpha(std::uint32_t c);
    bool IsAlNum(std::uint32_t c);
#endif

bool IsSymbolic(char c);

#endif


