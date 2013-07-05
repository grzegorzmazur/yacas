#include "lisperror.h"
#include "lispenvironment.h"
#include "standard.h"

void Handle(LispInt aError, LispEnvironment& aEnvironment, LispOutput& aOutput)
{
    if (aEnvironment.ErrorString(aError)[0] != '\0') {
        if (aEnvironment.iInputStatus.LineNumber() >= 0) {
            LispChar linenum[20];
            InternalIntToAscii(linenum,aEnvironment.iInputStatus.LineNumber());
            aOutput.Write(aEnvironment.iInputStatus.FileName());
            aOutput.Write("(");
            aOutput.Write(linenum);
            aOutput.Write(") : ");
        }
        aEnvironment.iCleanup.Delete();
        aOutput.Write(aEnvironment.ErrorString(aError));
        aOutput.Write("\n");
    }
}
