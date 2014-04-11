#include "yacas/lisperror.h"
#include "yacas/lispenvironment.h"
#include "yacas/standard.h"

void HandleError(const LispError& error, LispEnvironment& aEnvironment, LispOutput& aOutput)
{
    if (aEnvironment.iInputStatus.LineNumber() >= 0) {
        LispChar linenum[20];
        InternalIntToAscii(linenum,aEnvironment.iInputStatus.LineNumber());
        aOutput.Write(aEnvironment.iInputStatus.FileName());
        aOutput.Write("(");
        aOutput.Write(linenum);
        aOutput.Write(") : ");
    }
    aEnvironment.iCleanup.Delete();
    aOutput.Write(error.what());
    aOutput.Write("\n");
}
