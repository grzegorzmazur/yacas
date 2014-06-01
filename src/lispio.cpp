
#include "yacas/yacasprivate.h"
#include "yacas/lispio.h"

void InputStatus::SetTo(const LispChar * aFileName)
{
    iFileName = aFileName;
    iLineNumber=1;
}

void InputStatus::RestoreFrom(InputStatus& aPreviousStatus)
{
    iLineNumber = aPreviousStatus.iLineNumber;
    iFileName = aPreviousStatus.iFileName;
}

void LispOutput::Write(const LispChar * aString)
{
    while (*aString != '\0')
        PutChar(*aString++);
}

