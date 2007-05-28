
#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif

#if 0//HAVE_DLFCN_H //TODO. is elfdll used at all still?


#include <dlfcn.h>
#include <stdio.h>

#include "yacasprivate.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "platdll.h"

LispInt LtDll::Open(LispChar * aDllFile,LispEnvironment& aEnvironment)
{
#if HAVE_DLFCN_H
    iDllFileName = aDllFile;
#ifdef YACAS_DEBUG
printf("Trying to open [%s]\n",aDllFile);
#endif
    handle = dlopen(aDllFile,RTLD_LAZY);
    if (handle == NULL)
    {
      char tempbuf[256];
#ifdef HAVE_VSNPRINTF
      snprintf(tempbuf,256,"%s/%s.so",PLUGIN_DIR,aDllFile);
#else
      sprintf(tempbuf,"%s/%s.so",PLUGIN_DIR,aDllFile);
#endif
      handle = dlopen(tempbuf,RTLD_LAZY);

    }

    if (handle)
    {
#ifdef YACAS_DEBUG
printf("handle opened\n");
#endif
        iPlugin = GetPlugin(aDllFile);
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
#if HAVE_DLFCN_H
        dlclose(handle);
#endif
    }
    handle = NULL;
}
LispPluginBase* LtDll::GetPlugin(LispChar * aDllFile)
{
#if HAVE_DLFCN_H
    LISPASSERT(handle != NULL);
    LispPluginBase* (*maker)(void);
    char buf[1024];
    //TODO potential buffer overflow!
    sprintf(buf,"make_%s",aDllFile);
    maker = (LispPluginBase*(*)(void))dlsym(handle,buf);
    return maker();
#endif
    return NULL;
}

#endif //HAVE_DLFCN_H
