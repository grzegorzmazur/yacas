

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

#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)



static void FlGraphStart(LispEnvironment& aEnvironment,LispInt aStackTop)
{
    graphEnvironment = &aEnvironment;
    graph.Set(ARGUMENT(1).Get());
    extern Fl_Tabs* mainTabs;
    extern Fl_Group* grapher;
    mainTabs->value(grapher);
    InternalTrue(aEnvironment,RESULT);
}




void AddGraphingCapabilities(LispEnvironment& aEnvironment)
{

#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) aEnvironment.SetCommand(fname,iname,nrargs,flags)
CORE_KERNEL_FUNCTION("FlGraphStart",FlGraphStart,1,YacasEvaluator::Macro | YacasEvaluator::Fixed);
#undef CORE_KERNEL_FUNCTION
/*
  aEnvironment.SetCommand(FlGraphStart, "FlGraphStart");
*/
  FltkgraphPlugin fl_plugin;
  fl_plugin.Add(aEnvironment);
}





