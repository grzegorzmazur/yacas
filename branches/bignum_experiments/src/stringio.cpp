

#include "yacasprivate.h"
#include "stringio.h"


StringInput::StringInput(LispString& aString,InputStatus& aStatus)
    : LispInput(aStatus),iString(aString.c_str(),LispFalse),  iCurrent(0)
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

LispBoolean StringInput::EndOfStream()
{
    return (iString[ iCurrent ] == '\0');
}

LispChar * StringInput::StartPtr()
{
    return iString.c_str();
}
LispInt StringInput::Position()
{
    return iCurrent;
}
void StringInput::SetPosition(LispInt aPosition)
{
  LISPASSERT(aPosition>=0);
  LISPASSERT(aPosition<iString.Size());
  iCurrent = aPosition;
}


StringOutput::StringOutput(LispString& aString) : iString(aString) { }

StringOutput::~StringOutput()
{
}

void StringOutput::PutChar(LispChar aChar)
{
    iString[iString.Size()-1]=aChar;
    iString.Append('\0');
}


