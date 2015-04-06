#include "yacas/lisperror.h"
#include "yacas/lispenvironment.h"
#include "yacas/standard.h"

void HandleError(const LispError& error, LispEnvironment& aEnvironment, std::ostream& aOutput)
{
    if (aEnvironment.iInputStatus.LineNumber() >= 0) {
        aOutput << aEnvironment.iInputStatus.FileName();
        aOutput << "(";
        aOutput << aEnvironment.iInputStatus.LineNumber();
        aOutput << ") : ";
    }
    //aEnvironment.iCleanup.Delete();
    aOutput << error.what() << '\n';
}
