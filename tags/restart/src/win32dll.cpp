
#include <windows.h>

#include "lisptype.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "platdll.h"

LispInt Win32Dll::Open(LispCharPtr aDllFile)
{
    handle = LoadLibrary(aDllFile);
    return (handle != NULL);
}

Win32Dll::~Win32Dll()
{
    if (handle)
        FreeLibrary((HMODULE) handle);
    handle = NULL;
}
LispPluginBase* Win32Dll::GetPlugin(void)
{
    LISPASSERT(handle != NULL);
    LispPluginBase* (*maker)(void);
    maker = (LispPluginBase*(*)(void))LoadLibrary((HMODULE)
handle,"maker");
    return maker();
}

