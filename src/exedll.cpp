#include "plugins_available.h"
#include "yacasprivate.h"
#include "lispenvironment.h"
#include "lispplugin.h"
#include "lispassert.h"
#include "exedll.h"
#include "errors.h"


#ifdef YACAS_DEBUG
#include <stdio.h>  // Safe, only included if YACAS_DEBUG is defined
#endif

struct ExeDllEntry
{
  char *name;
  LispPluginBase* (*maker)(void);
};

#ifdef EXE_DLL_PLUGINS
extern "C" {
extern LispPluginBase* make_pcre(void);
extern LispPluginBase* make_filescanner(void);
}
#endif

// This list has to be sorted alphabetically!
static const ExeDllEntry exeDllentries[] =
{
#ifdef EXE_DLL_PLUGINS
  {"filescanner",make_filescanner},
  {"pcre",make_pcre},
#endif
};

/*TODO here we should have code that checks, at least in debug mode, that the
  exedll entries are sorted alphabetically!
*/

ExePluginMaker FindExePlugin(char* aName)
{

  LispInt low=0, high=sizeof(exeDllentries)/sizeof(ExeDllEntry);
  LispInt mid;
  for(;;)
  {
    if (low>=high)
    {
      mid=-1;
      goto CONTINUE;
    }
    mid = (low+high)>>1;

    LispInt cmp = StrCompare(aName, exeDllentries[mid].name);
    if (cmp < 0)
    {
      high = mid;
    }
    else if (cmp > 0)
    {
      low = (++mid);
    }
    else
    {
      goto CONTINUE;
    }
  }
CONTINUE:
  if (mid>=0)
    return exeDllentries[mid].maker;
  return NULL;
}

ExeDll::~ExeDll()
{
}
LispInt ExeDll::Open(LispChar * aDllFile,LispEnvironment& aEnvironment)
{
    iDllFileName = aDllFile;
    DBG_printf("ExeDll::Open: Trying to open [%s]\n",aDllFile);
    if (iMaker)
    {
        DBG_printf("ExeDll::Open: handle opened\n");
        iPlugin = GetPlugin(aDllFile);
        if (iPlugin)
        {
            DBG_printf("ExeDll::Open: plugin found\n");
            iPlugin->Add(aEnvironment);
        }
    }
    return (iMaker && iPlugin);
}

LispInt ExeDll::Close(LispEnvironment& aEnvironment)
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
LispPluginBase* ExeDll::GetPlugin(LispChar * aDllFile)
{
  LISPASSERT(iMaker);
  if (!iMaker)
  {
    RaiseError("ExeDll::OpenGetPlugin error");
  }
  return iMaker();
}

