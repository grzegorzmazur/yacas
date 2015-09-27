/** \file stringio.h
 * definitions of input output classes that read and write from string.
 */

#ifndef YACAS_STRINGIO_H
#define YACAS_STRINGIO_H

#include "lispio.h"
#include "utf8.h"

#include <string>

class StringInput : public LispInput
{
public:
    StringInput(const std::string&, InputStatus&);
    char32_t Next() override;
    char32_t Peek() override;
    bool EndOfStream() const override;
    std::size_t Position() const override;
    void SetPosition(std::size_t aPosition) override;
protected:
    std::string _string;
    std::string::const_iterator _current;
};

#endif

