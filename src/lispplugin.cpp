

#include "yacasprivate.h"
#include "lispenvironment.h"

#include "lispplugin.h"

LispDllBase::~LispDllBase()
{
}

/* default functionality fails to open a library */
LispInt LispDllBase::Open(LispCharPtr aDllFile)
{
    return 0;
}

/* default functionality fails to open a library */
LispPluginBase* LispDllBase::GetPlugin(void)
{
    return NULL;
}

