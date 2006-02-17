
#ifndef __elfdll_h__
#define __elfdll_h__

#include "yacasbase.h"
#include "lispplugin.h"

class ElfDll : public LispDllBase
{
public:
    ElfDll() : handle(NULL) {}
    virtual ~ElfDll();
    virtual LispInt Open(LispChar * aDllFile,LispEnvironment& aEnvironment);
    virtual LispInt Close(LispEnvironment& aEnvironment);
protected:
    virtual LispPluginBase* GetPlugin(LispChar * aDllFile);
private:
    void *handle;
};

#endif
