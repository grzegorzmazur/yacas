

#include <FL/Fl_Tabs.H>
#include <FL/fl_draw.H>
#include "yacasprivate.h"
#include "lisptype.h"
#include "lispenvironment.h"
#include "lispatom.h"
#include "standard.h"
#include "arggetter.h"
#include "lispplugin.h"
#include "platmath.h"
#include "genericstructs.h"
#include "errors.h"

#include "fltkgraphstub.cc"

LispEnvironment* graphEnvironment;
LispPtr graph;


static void FlGraphStart(LispEnvironment& aEnvironment, LispPtr& aResult,
                       LispPtr& aArguments)
{
    TESTARGS(2);
    graphEnvironment = &aEnvironment;
    graph.Set(Argument(aArguments,1).Get());
    extern Fl_Tabs* mainTabs;
    extern Fl_Group* grapher;
    mainTabs->value(grapher);
    InternalTrue(aEnvironment,aResult);
}




void AddGraphingCapabilities(LispEnvironment& aEnvironment)
{
  aEnvironment.SetCommand(FlGraphStart, "FlGraphStart");
  ThisPlugin fl_plugin;
  fl_plugin.Add(aEnvironment);
}





