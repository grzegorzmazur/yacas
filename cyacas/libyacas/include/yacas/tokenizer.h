/** \file tokenizer.h
 * definitions of input output classes that read and write from string.
 */


#ifndef YACAS_TOKENIZER_H
#define YACAS_TOKENIZER_H

#include "lispio.h"

#include <cctype>
#include <cstdint>
#include <string>

class LispTokenizer {
public:
    virtual ~LispTokenizer() = default;

    /// NextToken returns a string representing the next token,
    /// or an empty list.
    virtual std::string NextToken(LispInput& aInput);
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
