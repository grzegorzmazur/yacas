
#ifndef __elfdll_h__
#define __elfdll_h__

#include "yacasbase.h"
#include "lispplugin.h"

class ElfDll : public LispDllBase
{
public:
    ElfDll() : handle(NULL) {}
    virtual ~ElfDll();
    virtual LispInt Open(LispCharPtr aDllFile);
    virtual LispPluginBase* GetPlugin(void);
private:
    void *handle;
};

#endif
