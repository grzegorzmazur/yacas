
#include <dlfcn.h>
#include <stdio.h>

#include "lisptype.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "elfdll.h"

LispInt ElfDll::Open(LispCharPtr aDllFile)
{
    handle = dlopen(aDllFile,RTLD_LAZY);
    return (handle != NULL);
}

ElfDll::~ElfDll()
{
    if (handle)
        dlclose(handle);
    handle = NULL;
}
LispPluginBase* ElfDll::GetPlugin(void)
{
    LISPASSERT(handle != NULL);
    LispPluginBase* (*maker)(void);
    maker = (LispPluginBase*(*)(void))dlsym(handle,"maker");
    return maker();
}
