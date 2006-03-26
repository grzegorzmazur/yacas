
#ifndef __ltdll_h__
#define __ltdll_h__

#include "yacasbase.h"
#include "lispplugin.h"

class LtDll : public LispDllBase
{
public:
    LtDll() : handle(NULL) {}
    virtual ~LtDll();	// TODO: woof -- Holy-Moly!
    virtual LispInt Open(LispChar * aDllFile,LispEnvironment& aEnvironment);
    virtual LispInt Close(LispEnvironment& aEnvironment);
protected:
    virtual LispPluginBase* GetPlugin(LispChar * aDllFile);
private:
    void *handle;
};

#endif
