
/* This file was automatically generated with cstubgen.
*/
#include "lisptype.h"
#include "lispenvironment.h"
#include "lispatom.h"
#include "standard.h"
#include "arggetter.h"
#include "lispplugin.h"
#include "platmath.h"
#include "stubs.h"
#include "genericstructs.h"





static void base_fl_color(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  ShortIntegerArgument(arg1,  1 );
  ShortIntegerArgument(arg2,  2 );
  ShortIntegerArgument(arg3,  3 );
  

  /* Call the actual function. */
 fl_color(arg1, arg2, arg3);

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_font(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  ShortIntegerArgument(arg1,  1 );
  ShortIntegerArgument(arg2,  2 );
  

  /* Call the actual function. */
 fl_font(arg1, arg2);

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_draw(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  InpStringArgument(arg1, 1);
  ShortIntegerArgument(arg2,  2 );
  ShortIntegerArgument(arg3,  3 );
  

  /* Call the actual function. */
 fl_draw(arg1, arg2, arg3);

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_height(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
int r =  fl_height();

/* Return result. */
  ReturnShortInteger(aEnvironment,aEnvironment.iStack.GetElement(aStackTop),r);
}

static void base_fl_descent(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
int r =  fl_descent();

/* Return result. */
  ReturnShortInteger(aEnvironment,aEnvironment.iStack.GetElement(aStackTop),r);
}

static void base_fl_width(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  InpStringArgument(arg1, 1);
  

  /* Call the actual function. */
double r =  fl_width(arg1);

/* Return result. */
  ReturnDoubleFloat(aEnvironment,aEnvironment.iStack.GetElement(aStackTop),r);
}

static void base_fl_push_matrix(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
 fl_push_matrix();

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_pop_matrix(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
 fl_pop_matrix();

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_scale(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  DoubleFloatArgument(arg1, 1);
  DoubleFloatArgument(arg2, 2);
  

  /* Call the actual function. */
 fl_scale(arg1, arg2);

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_translate(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  DoubleFloatArgument(arg1, 1);
  DoubleFloatArgument(arg2, 2);
  

  /* Call the actual function. */
 fl_translate(arg1, arg2);

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_rotate(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  DoubleFloatArgument(arg1, 1);
  

  /* Call the actual function. */
 fl_rotate(arg1);

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_begin_points(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
 fl_begin_points();

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_begin_line(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
 fl_begin_line();

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_begin_loop(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
 fl_begin_loop();

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_begin_polygon(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
 fl_begin_polygon();

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_vertex(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  DoubleFloatArgument(arg1, 1);
  DoubleFloatArgument(arg2, 2);
  

  /* Call the actual function. */
 fl_vertex(arg1, arg2);

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_end_points(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
 fl_end_points();

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_end_line(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
 fl_end_line();

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_end_loop(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
 fl_end_loop();

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}

static void base_fl_end_polygon(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  /* Obtain arguments passed in. */
  

  /* Call the actual function. */
 fl_end_polygon();

/* Return result. */
  InternalTrue(aEnvironment,aEnvironment.iStack.GetElement(aStackTop));
}
 


class FltkgraphPlugin : public LispPluginBase
{
public:
    virtual void Add(LispEnvironment& aEnvironment);
    virtual void Remove(LispEnvironment& aEnvironment);
};
void FltkgraphPlugin::Add(LispEnvironment& aEnvironment)
{
  SetShortIntegerConstant(aEnvironment, "FlHelvetica", FL_HELVETICA);
  SetShortIntegerConstant(aEnvironment, "FlHelveticaBold", FL_HELVETICA_BOLD);
  SetShortIntegerConstant(aEnvironment, "FlHelveticaItalic", FL_HELVETICA_ITALIC);
  SetShortIntegerConstant(aEnvironment, "FlHelveticaBoldItalic", FL_HELVETICA_BOLD_ITALIC);
  SetShortIntegerConstant(aEnvironment, "FlCourier", FL_COURIER);
  SetShortIntegerConstant(aEnvironment, "FlCourierBold", FL_COURIER_BOLD);
  SetShortIntegerConstant(aEnvironment, "FlCourierItalic", FL_COURIER_ITALIC);
  SetShortIntegerConstant(aEnvironment, "FlCourierBoldItalic", FL_COURIER_BOLD_ITALIC);
  SetShortIntegerConstant(aEnvironment, "FlTimes", FL_TIMES);
  SetShortIntegerConstant(aEnvironment, "FlTimesBold", FL_TIMES_BOLD);
  SetShortIntegerConstant(aEnvironment, "FlTimesItalic", FL_TIMES_ITALIC);
  SetShortIntegerConstant(aEnvironment, "FlTimesBoldItalic", FL_TIMES_BOLD_ITALIC);
  SetShortIntegerConstant(aEnvironment, "FlSymbol", FL_SYMBOL);
  SetShortIntegerConstant(aEnvironment, "FlScreen", FL_SCREEN);
  SetShortIntegerConstant(aEnvironment, "FlScreenBold", FL_SCREEN_BOLD);
  SetShortIntegerConstant(aEnvironment, "FlZapfDingbats", FL_ZAPF_DINGBATS);

  aEnvironment.SetCommand(base_fl_color, "FlColor",3,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_font, "FlFont",2,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_draw, "FlDraw",3,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_height, "FlHeight",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_descent, "FlDescent",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_width, "FlWidth",1,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_push_matrix, "FlPushMatrix",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_pop_matrix, "FlPopMatrix",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_scale, "FlScale",2,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_translate, "FlTranslate",2,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_rotate, "FlRotate",1,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_begin_points, "FlBeginPoints",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_begin_line, "FlBeginLine",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_begin_loop, "FlBeginLoop",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_begin_polygon, "FlBeginPolygon",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_vertex, "FlVertex",2,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_end_points, "FlEndPoints",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_end_line, "FlEndLine",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_end_loop, "FlEndLoop",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(base_fl_end_polygon, "FlEndPolygon",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
}

void FltkgraphPlugin::Remove(LispEnvironment& aEnvironment)
{
//printf("CLOSED DLL!!!\n");
  aEnvironment.RemoveCoreCommand("FlColor");
  aEnvironment.RemoveCoreCommand("FlFont");
  aEnvironment.RemoveCoreCommand("FlDraw");
  aEnvironment.RemoveCoreCommand("FlHeight");
  aEnvironment.RemoveCoreCommand("FlDescent");
  aEnvironment.RemoveCoreCommand("FlWidth");
  aEnvironment.RemoveCoreCommand("FlPushMatrix");
  aEnvironment.RemoveCoreCommand("FlPopMatrix");
  aEnvironment.RemoveCoreCommand("FlScale");
  aEnvironment.RemoveCoreCommand("FlTranslate");
  aEnvironment.RemoveCoreCommand("FlRotate");
  aEnvironment.RemoveCoreCommand("FlBeginPoints");
  aEnvironment.RemoveCoreCommand("FlBeginLine");
  aEnvironment.RemoveCoreCommand("FlBeginLoop");
  aEnvironment.RemoveCoreCommand("FlBeginPolygon");
  aEnvironment.RemoveCoreCommand("FlVertex");
  aEnvironment.RemoveCoreCommand("FlEndPoints");
  aEnvironment.RemoveCoreCommand("FlEndLine");
  aEnvironment.RemoveCoreCommand("FlEndLoop");
  aEnvironment.RemoveCoreCommand("FlEndPolygon");
}

extern "C" {
LispPluginBase* maker(void)
{
    return NEW FltkgraphPlugin;
}

};




