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
    StringInput(const LispString& aString, InputStatus& aStatus);
    virtual LispChar Next();
    virtual LispChar Peek();
    virtual bool EndOfStream() const;
    virtual const LispChar* StartPtr();
    virtual LispInt Position();
    virtual void SetPosition(LispInt aPosition);
protected:
    LispString iString;
    LispInt iCurrent;
};

class StringOutput : public LispOutput
{
public:
    StringOutput(LispString& aString);
    void PutChar(LispChar aChar);
public:
    LispString& iString;
};

inline
StringOutput::StringOutput(LispString& aString):
    iString(aString)
{
}

inline
void StringOutput::PutChar(LispChar aChar)
{
    iString.push_back(aChar);
}

#endif

