

#ifndef __exedll_h__
#define __exedll_h__

#include "yacasbase.h"
#include "lispplugin.h"

typedef LispPluginBase* (*ExePluginMaker)(void) ;


ExePluginMaker FindExePlugin(char* aName);

class ExeDll : public LispDllBase
{
public:
    ExeDll(LispPluginBase* (*aMaker)(void)) : iMaker(aMaker) {}
    virtual ~ExeDll();
    virtual LispInt Open(LispCharPtr aDllFile,LispEnvironment& aEnvironment);
    virtual LispInt Close(LispEnvironment& aEnvironment);
protected:
    virtual LispPluginBase* GetPlugin(void);
private:
    LispPluginBase* (*iMaker)(void);
};


#endif
