
#include "yacas/yacasprivate.h"
#include "yacas/yacasbase.h"
#include "yacas/lispio.h"
#include "yacas/standard.h"
#include "yacas/lispenvironment.h"

static LispInt FindMarkerBegin(const LispChar* aPtr, LispInt aFrom)
{
REDO:
    while (aPtr[aFrom] && aPtr[aFrom] != '<') aFrom++;

    if (aPtr[aFrom] == '\0')
        return aFrom;

    if (aPtr[aFrom+1] == '?')
        return aFrom;
    aFrom+=2;
    goto REDO;
}
static LispInt FindMarkerEnd(const LispChar* aPtr, LispInt aFrom)
{
    for (;;) {
        while (aPtr[aFrom] && aPtr[aFrom] != '?')
            aFrom++;

        if (aPtr[aFrom] == '\0')
            return aFrom;

        if (aPtr[aFrom+1] == '>')
            return aFrom;

        aFrom+=2;
    }
}

static LispInt FindEndAscii(const LispChar* aPtr, LispInt aFrom)
{
    return FindMarkerBegin(aPtr,aFrom);
}
static LispInt FindEndCommand(const LispChar* aPtr, LispInt aFrom)
{
    return FindMarkerEnd(aPtr,aFrom);
}

/** PatchLoad: patch a file, and write to current output.
 *  Everything between <? and ?> is evaluated. The result
 *  is thrown away.
 */
void PatchLoad(const LispChar* aFileContent, std::ostream& aOutput,
               LispEnvironment& aEnvironment)
{
    LispInt i=0;
    LispInt next;

REDO:
    next = FindEndAscii(aFileContent,i);
    while (i<next)
    {
        aOutput.put(aFileContent[i]);
        i++;
    }
    if (aFileContent[i] == '<')
    {
        i+=2;

        next = FindEndCommand(aFileContent,i);

        const std::string content(aFileContent + i, next - i);

        InputStatus oldstatus = aEnvironment.iInputStatus;
        aEnvironment.iInputStatus.SetTo("String");

        StringInput newInput(content,aEnvironment.iInputStatus);
        LispLocalInput localInput(aEnvironment, &newInput);

        DoInternalLoad(aEnvironment,&newInput);

        aEnvironment.iInputStatus.RestoreFrom(oldstatus);

        i=next;
        if (aFileContent[i] == '?')
        {
            i+=2;
            goto REDO;
        }
    }
}


