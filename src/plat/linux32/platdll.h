
#ifndef __ltdll_h__
#define __ltdll_h__

#include "../../lispplugin.h"

class ElfDll : public LispDllBase
{
public:
    ElfDll() : handle(NULL) {}
    virtual ~ElfDll();
    virtual LispInt Open(LispCharPtr aDllFile, LispEnvironment& aEnvironment);
    virtual LispInt Close(LispEnvironment& aEnvironment);
    virtual LispPluginBase* GetPlugin(void);
private:
    void *handle;
};

#endif
