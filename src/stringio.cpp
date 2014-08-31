

#include "yacas/yacasprivate.h"
#include "yacas/stringio.h"


StringInput::StringInput(const LispString& aString, InputStatus& aStatus):
    LispInput(aStatus),
    iString(aString),
    iCurrent(0)
{
}

LispChar StringInput::Next()
{
    LispChar result = iString[ iCurrent ];

    if (!EndOfStream())
        iCurrent++;
    else if (result == '\n')
        iStatus.NextLine();

    return result;
}

LispChar StringInput::Peek()
{
    return iString[ iCurrent ];
}

bool StringInput::EndOfStream() const
{
    return (iString[ iCurrent ] == '\0');
}

const LispChar* StringInput::StartPtr()
{
    return iString.c_str();
}

std::size_t StringInput::Position() const
{
    return iCurrent;
}

void StringInput::SetPosition(std::size_t aPosition)
{
  assert(aPosition<iString.size());
  iCurrent = aPosition;
}
