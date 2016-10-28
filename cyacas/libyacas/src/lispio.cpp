
#include "yacas/lispio.h"

void InputStatus::SetTo(const std::string& aFileName)
{
    iFileName = aFileName;
    iLineNumber=1;
}

void InputStatus::RestoreFrom(InputStatus& aPreviousStatus)
{
    iLineNumber = aPreviousStatus.iLineNumber;
    iFileName = aPreviousStatus.iFileName;
}
