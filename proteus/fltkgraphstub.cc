
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



static void base_fl_color(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  ShortIntegerArgument(g, arg2, LispTrue);
  ShortIntegerArgument(g, arg3, LispTrue);
  g.Finalize(3);

  /* Call the actual function. */
 fl_color(arg1, arg2, arg3);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_font(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  ShortIntegerArgument(g, arg2, LispTrue);
  g.Finalize(2);

  /* Call the actual function. */
 fl_font(arg1, arg2);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_draw(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  InpStringArgument(g,arg1,LispTrue);
  ShortIntegerArgument(g, arg2, LispTrue);
  ShortIntegerArgument(g, arg3, LispTrue);
  g.Finalize(3);

  /* Call the actual function. */
 fl_draw(arg1, arg2, arg3);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_height(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
int r =  fl_height();

/* Return result. */
  ReturnShortInteger(aEnvironment,aResult,r);
}

static void base_fl_descent(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
int r =  fl_descent();

/* Return result. */
  ReturnShortInteger(aEnvironment,aResult,r);
}

static void base_fl_width(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  InpStringArgument(g,arg1,LispTrue);
  g.Finalize(1);

  /* Call the actual function. */
double r =  fl_width(arg1);

/* Return result. */
  ReturnDoubleFloat(aEnvironment,aResult,r);
}

static void base_fl_push_matrix(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 fl_push_matrix();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_pop_matrix(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 fl_pop_matrix();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_scale(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  g.Finalize(2);

  /* Call the actual function. */
 fl_scale(arg1, arg2);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_translate(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  g.Finalize(2);

  /* Call the actual function. */
 fl_translate(arg1, arg2);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_rotate(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  g.Finalize(1);

  /* Call the actual function. */
 fl_rotate(arg1);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_begin_points(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 fl_begin_points();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_begin_line(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 fl_begin_line();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_begin_loop(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 fl_begin_loop();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_begin_polygon(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 fl_begin_polygon();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_vertex(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  g.Finalize(2);

  /* Call the actual function. */
 fl_vertex(arg1, arg2);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_end_points(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 fl_end_points();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_end_line(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 fl_end_line();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_end_loop(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 fl_end_loop();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_fl_end_polygon(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 fl_end_polygon();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}
 


class ThisPlugin : public LispPluginBase
{
public:
    virtual void Add(LispEnvironment& aEnvironment);
    virtual void Remove(LispEnvironment& aEnvironment);
};
void ThisPlugin::Add(LispEnvironment& aEnvironment)
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

  aEnvironment.SetCommand(base_fl_color, "FlColor");
  aEnvironment.SetCommand(base_fl_font, "FlFont");
  aEnvironment.SetCommand(base_fl_draw, "FlDraw");
  aEnvironment.SetCommand(base_fl_height, "FlHeight");
  aEnvironment.SetCommand(base_fl_descent, "FlDescent");
  aEnvironment.SetCommand(base_fl_width, "FlWidth");
  aEnvironment.SetCommand(base_fl_push_matrix, "FlPushMatrix");
  aEnvironment.SetCommand(base_fl_pop_matrix, "FlPopMatrix");
  aEnvironment.SetCommand(base_fl_scale, "FlScale");
  aEnvironment.SetCommand(base_fl_translate, "FlTranslate");
  aEnvironment.SetCommand(base_fl_rotate, "FlRotate");
  aEnvironment.SetCommand(base_fl_begin_points, "FlBeginPoints");
  aEnvironment.SetCommand(base_fl_begin_line, "FlBeginLine");
  aEnvironment.SetCommand(base_fl_begin_loop, "FlBeginLoop");
  aEnvironment.SetCommand(base_fl_begin_polygon, "FlBeginPolygon");
  aEnvironment.SetCommand(base_fl_vertex, "FlVertex");
  aEnvironment.SetCommand(base_fl_end_points, "FlEndPoints");
  aEnvironment.SetCommand(base_fl_end_line, "FlEndLine");
  aEnvironment.SetCommand(base_fl_end_loop, "FlEndLoop");
  aEnvironment.SetCommand(base_fl_end_polygon, "FlEndPolygon");
}

void ThisPlugin::Remove(LispEnvironment& aEnvironment)
{
//printf("CLOSED DLL!!!\n");
  aEnvironment.RemoveCommand("FlColor");
  aEnvironment.RemoveCommand("FlFont");
  aEnvironment.RemoveCommand("FlDraw");
  aEnvironment.RemoveCommand("FlHeight");
  aEnvironment.RemoveCommand("FlDescent");
  aEnvironment.RemoveCommand("FlWidth");
  aEnvironment.RemoveCommand("FlPushMatrix");
  aEnvironment.RemoveCommand("FlPopMatrix");
  aEnvironment.RemoveCommand("FlScale");
  aEnvironment.RemoveCommand("FlTranslate");
  aEnvironment.RemoveCommand("FlRotate");
  aEnvironment.RemoveCommand("FlBeginPoints");
  aEnvironment.RemoveCommand("FlBeginLine");
  aEnvironment.RemoveCommand("FlBeginLoop");
  aEnvironment.RemoveCommand("FlBeginPolygon");
  aEnvironment.RemoveCommand("FlVertex");
  aEnvironment.RemoveCommand("FlEndPoints");
  aEnvironment.RemoveCommand("FlEndLine");
  aEnvironment.RemoveCommand("FlEndLoop");
  aEnvironment.RemoveCommand("FlEndPolygon");
}

extern "C" {
LispPluginBase* maker(void)
{
    return new ThisPlugin;
}

};

