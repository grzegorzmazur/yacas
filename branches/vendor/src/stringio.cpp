

#include "stringio.h"


StringInput::StringInput(const LispString aString,InputStatus& aStatus)
    : LispInput(aStatus)
{
    iString = aString.String();
    iCurrent = 0;
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

LispCharPtr StringInput::StartPtr()
{
    return iString.String();
}
LispInt StringInput::Position()
{
    return iCurrent;
}


StringOutput::StringOutput(LispString& aString) : iString(aString) { }

StringOutput::~StringOutput()
{
}

void StringOutput::PutChar(LispChar aChar)
{
    iString[iString.NrItems()-1]=aChar;
    iString.Append('\0');
}


