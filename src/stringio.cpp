

#include "yacas/yacasprivate.h"
#include "yacas/stringio.h"


StringInput::StringInput(LispString& aString,InputStatus& aStatus)
    : LispInput(aStatus),iString(aString.c_str()),  iCurrent(0)
{
}

LispChar StringInput::Next()
{
    LispChar result = iString[ iCurrent ];
    if (!EndOfStream())
    {
        iCurrent++;
    }
    else if (result == '\n')
        iStatus.NextLine();
    return result;
}

LispChar StringInput::Peek()
{
    return iString[ iCurrent ];
}

bool StringInput::EndOfStream()
{
    return (iString[ iCurrent ] == '\0');
}

const LispChar* StringInput::StartPtr()
{
    return iString.c_str();
}
LispInt StringInput::Position()
{
    return iCurrent;
}
void StringInput::SetPosition(LispInt aPosition)
{
  assert(aPosition>=0);
  assert(aPosition<iString.Size());
  iCurrent = aPosition;
}


StringOutput::StringOutput(LispString& aString) : iString(aString) { }

StringOutput::~StringOutput()
{
}

void StringOutput::PutChar(LispChar aChar)
{
    iString[iString.size()-1]=aChar;
    iString.push_back('\0');
}


