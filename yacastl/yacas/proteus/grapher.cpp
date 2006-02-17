

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
#include "FltkConsole.h"

#include "fltkgraphstub.cc"

LispEnvironment* graphEnvironment;
LispPtr graph;

#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)



static void FlGraphStart(LispEnvironment& aEnvironment,LispInt aStackTop)
{
  extern ConsoleOutBase* cell_to_insert;
  if (cell_to_insert)
  {
    delete cell_to_insert;
  }
  ShortIntegerArgument(width,  2 );
  ShortIntegerArgument(height, 3 );

  cell_to_insert = new ConsoleDrawer(aEnvironment,ARGUMENT(1),width,height);
  InternalTrue(aEnvironment,RESULT);

/*TODO remove?
    graphEnvironment = &aEnvironment;
    graph = (ARGUMENT(1));
    extern Fl_Tabs* mainTabs;
    extern Fl_Group* grapher;
    mainTabs->value(grapher);
    InternalTrue(aEnvironment,RESULT);
*/
}




void AddGraphingCapabilities(LispEnvironment& aEnvironment)
{

#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) aEnvironment.SetCommand(fname,iname,nrargs,flags)
CORE_KERNEL_FUNCTION("FlGraphStart",FlGraphStart,3,YacasEvaluator::Function | YacasEvaluator::Fixed);
#undef CORE_KERNEL_FUNCTION
/*
  aEnvironment.SetCommand(FlGraphStart, "FlGraphStart");
*/
  FltkgraphPlugin fl_plugin;
  fl_plugin.Add(aEnvironment);
}





