
#include "stubs.h"
#include "lispio.h"

void InputStatus::SetTo(LispCharPtr aFileName)
{
    iFileName = aFileName;
    iLineNumber=1;
}

void InputStatus::RestoreFrom(InputStatus& aPreviousStatus)
{
    iLineNumber = aPreviousStatus.iLineNumber;
    iFileName = aPreviousStatus.iFileName;
}


LispInput::~LispInput()
{
}

LispOutput::~LispOutput()
{
}

void LispOutput::Write(LispCharPtr aString)
{
    while (*aString != '\0')
        PutChar(*aString++);
}

