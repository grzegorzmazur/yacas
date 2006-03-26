
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>

#include "yacasprivate.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "platdll.h"

LispInt Win32Dll::Open(LispChar * aDllFile,LispEnvironment& aEnvironment)
{
    iDllFileName = aDllFile;
    handle = LoadLibrary(aDllFile);

    if (handle)
    {
        iPlugin = GetPlugin(aDllFile);
        if (iPlugin)
        {
            iPlugin->Add(aEnvironment);
        }
    }
    return (handle && iPlugin);
}

LispInt Win32Dll::Close(LispEnvironment& aEnvironment)
{
    if (iPlugin)
    {
        iPlugin->Remove(aEnvironment);
        delete iPlugin;
        iPlugin = NULL;
        return 1;
    }
    return 0;
}

Win32Dll::~Win32Dll()
{
    if (handle)
    {
        LISPASSERT(!iPlugin);
        FreeLibrary((HMODULE) handle);
    }
    handle = NULL;
}
LispPluginBase* Win32Dll::GetPlugin(LispChar * aDllFile)
{
    LISPASSERT(handle);
    LispPluginBase* (*maker)(void);
    char buf[1024];
    //TODO potential buffer overflow!
    sprintf(buf,"make_%s",aDllFile);
    maker = (LispPluginBase*(*)(void))GetProcAddress((HMODULE)handle,buf);
    return maker();
}

