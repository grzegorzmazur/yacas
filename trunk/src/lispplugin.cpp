

#include "yacasprivate.h"
#include "lispenvironment.h"

#include "lispplugin.h"


LispDllBase::LispDllBase()
{
    iPlugin = NULL;
}

LispDllBase::~LispDllBase()
{
}

/* default functionality fails to open a library */
LispInt LispDllBase::Open(LispCharPtr aDllFile, LispEnvironment& aEnvironment)
{
    return 0;
}
LispInt LispDllBase::Close(LispEnvironment& aEnvironment)
{
    return 0;
}

LispCharPtr LispDllBase::DllFileName() const
{
    return &iDllFileName[0];
}


LispPluginBase* LispDllBase::Plugin(void)
{
    return iPlugin;
}

/* default functionality fails to open a library */
LispPluginBase* LispDllBase::GetPlugin(void)
{
    return NULL;
}

