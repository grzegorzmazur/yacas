/** \file stringio.h
 * definitions of input output classes that read and write from string.
 */

#ifndef YACAS_STRINGIO_H
#define YACAS_STRINGIO_H

#include "yacasbase.h"
#include "lispio.h"
#include "lispstring.h"

class StringInput : public LispInput
{
public:
    StringInput(const std::string& aString, InputStatus& aStatus);
    virtual LispChar Next();
    virtual LispChar Peek();
    virtual bool EndOfStream() const;
    virtual std::size_t Position() const;
    virtual void SetPosition(std::size_t aPosition);
protected:
    std::string iString;
    std::size_t iCurrent;
};

#endif

