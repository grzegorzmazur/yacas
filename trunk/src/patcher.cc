
#include "lisptype.h"
#include "lispio.h"
#include "standard.h"
#include "lispenvironment.h"

static LispInt FindMarkerBegin(LispCharPtr aPtr, LispInt aFrom)
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
static LispInt FindMarkerEnd(LispCharPtr aPtr, LispInt aFrom)
{
REDO:
    while (aPtr[aFrom] && aPtr[aFrom] != '?') aFrom++;

    if (aPtr[aFrom] == '\0')
        return aFrom;

    if (aPtr[aFrom+1] == '>')
        return aFrom;
    aFrom+=2;
    goto REDO;
}

static LispInt FindEndAscii(LispCharPtr aPtr, LispInt aFrom)
{
    return FindMarkerBegin(aPtr,aFrom);
}
static LispInt FindEndCommand(LispCharPtr aPtr, LispInt aFrom)
{
    return FindMarkerEnd(aPtr,aFrom);
}

/** PatchLoad: patch a file, and write to current output.
 *  Everything between <? and ?> is evaluated. The result
 *  is thrown away.
 */
void PatchLoad(LispCharPtr aFileContent, LispOutput& aOutput,
               LispEnvironment& aEnvironment)
{
    LispInt i=0;
    LispInt next;

REDO:
    next = FindEndAscii(aFileContent,i);
    while (i<next)
    {
        aOutput.PutChar(aFileContent[i]);
        i++;
    }
    if (aFileContent[i] == '<')
    {
        i+=2;

        next = FindEndCommand(aFileContent,i);

        LispString content;

        content.GrowTo(next-i+1);
        {
            LispInt j;
            for (j=0;j<next-i;j++)
                content[j] = aFileContent[i+j];
            content[next-i] = '\0';
        }

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


