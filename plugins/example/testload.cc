/*
#include <dlfcn.h>
*/
#include <stdio.h>


#include "lisptype.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "elfdll.h"

#include "yacas.h"







int main(void)
{
    ElfDll dll;
	char * filename = "./libbareplugin.so";
        
    if (!dll.Open(filename))
    {
        printf("could not open dll!\n");
        return 1;
    }

    LispPluginBase* plugin = dll.GetPlugin(filename);
    if (plugin == NULL)
    {
        printf("could not find maker!\n");
        return 1;
    }
    CYacas* yacas = CYacas::NewL();

    plugin->Add(yacas->getDefEnv().getEnv());

    delete yacas;
        
/*
    void* (*maker)(void);
    void *handle = dlopen("./libbareplugin.so",RTLD_LAZY);

    if (handle == NULL)
    {
        printf("could not open dll!\n");
        return 1;
    }

    maker = (void*(*)(void))dlsym(handle,"maker");
    if (maker == NULL)
    {
        printf("could not find maker!\n");
        return 1;
    }
    maker();
    dlclose(handle);
    */
    return 0;
}
