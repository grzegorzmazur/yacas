#include <stdio.h>

#include "yacasprivate.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "platdll.h"
#include "ltdl.h"
#include "errors.h"

LispInt LtDll::Open(LispCharPtr aDllFile,LispEnvironment& aEnvironment)
{
    const char *err;

    iDllFileName = aDllFile;
#ifdef YACAS_DEBUG
    {
      extern int verbose_debug;
      if (verbose_debug)
        printf("LtDll::Open: Trying to open [%s]\n",aDllFile);
    }
#endif
    if (lt_dlinit() != 0)
    {
        err = lt_dlerror();
        if (err) 
        {
          RaiseError("LtDll::Open: lt_dlinit says %s\n",err);
        }
    }
    for (LispInt i=0; i<aEnvironment.iDllDirectories.NrItems(); i++)
        lt_dladdsearchdir(aEnvironment.iDllDirectories[i]->String());
    handle = lt_dlopenext(aDllFile);
    if (handle)
    {
#ifdef YACAS_DEBUG
        extern int verbose_debug;
        if (verbose_debug)
          printf("LtDll::Open: handle opened\n");
#endif
        iPlugin = GetPlugin(aDllFile);
        if (iPlugin)
        {
#ifdef YACAS_DEBUG
          extern int verbose_debug;
          if (verbose_debug)
            printf("LtDll::Open: plugin found\n");
#endif
          iPlugin->Add(aEnvironment);
        }
    } 
    else
    {
        err = lt_dlerror();
        if (err) 
        {
          RaiseError("LtDll::Open: lt_dlopen says %s\n",err);
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
//    const char* err;

    if (handle)
    {
        LISPASSERT(iPlugin == NULL);
/*        
        FIXME: We want to unload the DLL, but before we can do so, we must
               destruct all the objects created by the DLL.

	if (lt_dlclose((lt_dlhandle)handle) != 0)
    	{
    	    err = lt_dlerror();
    	    if (err) printf("LtDll::~LtDll: lt_dlclose says %s\n",err);
    	}
*/
    }
    handle = NULL;
}

LispPluginBase* LtDll::GetPlugin(LispCharPtr aDllFile)
{
    const char* err;

    LISPASSERT(handle != NULL);
    LispPluginBase* (*maker)(void);
    char buf[1024];
    //TODO potential buffer overflow!
    sprintf(buf,"make_%s",aDllFile);
    maker = (LispPluginBase*(*)(void))lt_dlsym((lt_dlhandle)handle,buf);
    if (!maker)
    {
        err = lt_dlerror();
	if (err) printf("LtDll::OpenGetPlugin: lt_dlsym says %s\n",err);
    }
    /* lt_dlexit(); */
    return maker();
}
