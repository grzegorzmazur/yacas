/** \file stringio.h
 * definitions of input output classes that read and write from string.
 */


#ifndef __stringio_h__
#define __stringio_h__

#include "yacasbase.h"
#include "lispio.h"
#include "lispstring.h"

class StringInput : public LispInput
{
public:
    StringInput(const LispString aString,InputStatus& aStatus);
    virtual LispChar Next();
    virtual LispChar Peek();
    virtual LispBoolean EndOfStream();
    virtual LispCharPtr StartPtr();
    virtual LispInt Position();
protected:
    LispString iString;
    LispInt iCurrent;
};

class StringOutput : public LispOutput
{
public:
    StringOutput(LispString& aString);
    virtual ~StringOutput();
    virtual void PutChar(LispChar aChar);
public:
    LispString& iString;
};



#endif

