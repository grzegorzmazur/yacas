
#if 1

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif

#if HAVE_DLFCN_H
#include <dlfcn.h>
#endif
#include <stdio.h>

#include "yacasprivate.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "platdll.h"

LispInt ElfDll::Open(LispCharPtr aDllFile,LispEnvironment& aEnvironment)
{
#if HAVE_DLFCN_H
    iDllFileName = aDllFile;
#ifdef YACAS_DEBUG
printf("Trying to open [%s]\n",aDllFile);
#endif
    handle = dlopen(aDllFile,RTLD_LAZY);
    if (handle)
    {
#ifdef YACAS_DEBUG
printf("handle opened\n");
#endif
        iPlugin = GetPlugin();
        if (iPlugin)
        {
#ifdef YACAS_DEBUG
printf("plugin found\n");
#endif
            iPlugin->Add(aEnvironment);
        }
    }
    return (handle != NULL && iPlugin != NULL);
#endif
    return NULL;
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
#if HAVE_DLFCN_H
        dlclose(handle);
#endif
    }
    handle = NULL;
}
LispPluginBase* ElfDll::GetPlugin(void)
{
#if HAVE_DLFCN_H
    LISPASSERT(handle != NULL);
    LispPluginBase* (*maker)(void);
    maker = (LispPluginBase*(*)(void))dlsym(handle,"maker");
    return maker();
#endif
    return NULL;
}

#endif
