
#include <stdio.h>

#include "yacasprivate.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "platdll.h"
#include "ltdl.h"

LispInt LtDll::Open(LispCharPtr aDllFile,LispEnvironment& aEnvironment)
{
    iDllFileName = aDllFile;
    lt_dlinit();
    handle = lt_dlopen(aDllFile/*,RTLD_LAZY*/);
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
LispInt LtDll::Close(LispEnvironment& aEnvironment)
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

LtDll::~LtDll()
{
    if (handle)
    {
        LISPASSERT(iPlugin == NULL);
        lt_dlclose(handle);
    }
    handle = NULL;
}
LispPluginBase* LtDll::GetPlugin(void)
{
    LISPASSERT(handle != NULL);
    LispPluginBase* (*maker)(void);
    maker = (LispPluginBase*(*)(void))lt_dlsym(handle,"maker");
    lt_dlexit();
    return maker();
}

