
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
printf("Trying to open [%s]\n",aDllFile);
    lt_dlinit();
    handle = lt_dlopen(aDllFile/*,RTLD_LAZY*/);
    if (handle)
    {
printf("handle opened\n");

        iPlugin = GetPlugin();
        if (iPlugin)
        {

printf("plugin found\n");
            iPlugin->Add(aEnvironment);
        }
    }

{
const char *err = lt_dlerror();
if (err) printf("Last error: %s\n",err);
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
        lt_dlclose((lt_dlhandle)handle);
    }
    handle = NULL;
}

LispPluginBase* LtDll::GetPlugin(void)
{
    LISPASSERT(handle != NULL);
    LispPluginBase* (*maker)(void);
    maker = (LispPluginBase*(*)(void))lt_dlsym((lt_dlhandle)handle,"maker");
    /* lt_dlexit(); */
    return maker();
}

