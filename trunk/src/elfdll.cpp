
#if 0 //sorry guys, I'm testing it on Mac OS X now ;-)

#include <dlfcn.h>
#include <stdio.h>

#include "yacasprivate.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "platdll.h"

LispInt ElfDll::Open(LispCharPtr aDllFile,LispEnvironment& aEnvironment)
{
    iDllFileName = aDllFile;
    handle = dlopen(aDllFile,RTLD_LAZY);
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
LispInt ElfDll::Close(LispEnvironment& aEnvironment)
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

ElfDll::~ElfDll()
{
    if (handle)
    {
        LISPASSERT(iPlugin == NULL);
        dlclose(handle);
    }
    handle = NULL;
}
LispPluginBase* ElfDll::GetPlugin(void)
{
    LISPASSERT(handle != NULL);
    LispPluginBase* (*maker)(void);
    maker = (LispPluginBase*(*)(void))dlsym(handle,"maker");
    return maker();
}

#endif
