
#ifndef __ltdll_h__
#define __ltdll_h__

#include "yacasbase.h"
#include "lispplugin.h"

class LtDll : public LispDllBase
{
public:
    LtDll() : handle(NULL) {}
    virtual ~LtDll();
    virtual LispInt Open(LispCharPtr aDllFile,LispEnvironment& aEnvironment);
    virtual LispInt Close(LispEnvironment& aEnvironment);
protected:
    virtual LispPluginBase* GetPlugin(void);
private:
    void *handle;
};

#endif
