
#include <windows.h>

#include "yacasprivate.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "platdll.h"

LispInt Win32Dll::Open(LispCharPtr aDllFile,LispEnvironment& aEnvironment)
{
    iDllFileName = aDllFile;
    handle = LoadLibrary(aDllFile);

    if (handle)
    {
        iPlugin = GetPlugin();
        if (iPlugin)
        {
            iPlugin->Add(aEnvironment);
        }
    }
    return (handle != NULL && iPlugin != NULL);
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
        LISPASSERT(iPlugin == NULL);
        FreeLibrary((HMODULE) handle);
    }
    handle = NULL;
}
LispPluginBase* Win32Dll::GetPlugin(void)
{
    LISPASSERT(handle != NULL);
    LispPluginBase* (*maker)(void);
    maker = (LispPluginBase*(*)(void))GetProcAddress((HMODULE)handle,"maker");
    return maker();
}

