
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

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include "glutcode.h"


static void base_glBegin(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  g.Finalize(1);

  /* Call the actual function. */
 glBegin(arg1);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glVertex3d(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  DoubleFloatArgument(g,arg3,LispTrue);
  g.Finalize(3);

  /* Call the actual function. */
 glVertex3d(arg1, arg2, arg3);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glEnd(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 glEnd();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glClearColor(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  DoubleFloatArgument(g,arg3,LispTrue);
  DoubleFloatArgument(g,arg4,LispTrue);
  g.Finalize(4);

  /* Call the actual function. */
 glClearColor(arg1, arg2, arg3, arg4);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glClear(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  g.Finalize(1);

  /* Call the actual function. */
 glClear(arg1);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glColor4d(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  DoubleFloatArgument(g,arg3,LispTrue);
  DoubleFloatArgument(g,arg4,LispTrue);
  g.Finalize(4);

  /* Call the actual function. */
 glColor4d(arg1, arg2, arg3, arg4);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glFrontFace(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  g.Finalize(1);

  /* Call the actual function. */
 glFrontFace(arg1);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glPointSize(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  g.Finalize(1);

  /* Call the actual function. */
 glPointSize(arg1);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glLineWidth(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  g.Finalize(1);

  /* Call the actual function. */
 glLineWidth(arg1);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glEnable(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  g.Finalize(1);

  /* Call the actual function. */
 glEnable(arg1);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glDisable(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  g.Finalize(1);

  /* Call the actual function. */
 glDisable(arg1);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glPushMatrix(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 glPushMatrix();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glPopMatrix(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 glPopMatrix();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glLoadIdentity(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 glLoadIdentity();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glRotated(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  DoubleFloatArgument(g,arg3,LispTrue);
  DoubleFloatArgument(g,arg4,LispTrue);
  g.Finalize(4);

  /* Call the actual function. */
 glRotated(arg1, arg2, arg3, arg4);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glScaled(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  DoubleFloatArgument(g,arg3,LispTrue);
  g.Finalize(3);

  /* Call the actual function. */
 glScaled(arg1, arg2, arg3);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glTranslated(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  DoubleFloatArgument(g,arg3,LispTrue);
  g.Finalize(3);

  /* Call the actual function. */
 glTranslated(arg1, arg2, arg3);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glNormal3d(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  DoubleFloatArgument(g,arg3,LispTrue);
  g.Finalize(3);

  /* Call the actual function. */
 glNormal3d(arg1, arg2, arg3);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glMatrixMode(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  g.Finalize(1);

  /* Call the actual function. */
 glMatrixMode(arg1);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glOrtho(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  DoubleFloatArgument(g,arg3,LispTrue);
  DoubleFloatArgument(g,arg4,LispTrue);
  DoubleFloatArgument(g,arg5,LispTrue);
  DoubleFloatArgument(g,arg6,LispTrue);
  g.Finalize(6);

  /* Call the actual function. */
 glOrtho(arg1, arg2, arg3, arg4, arg5, arg6);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glFrustum(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  DoubleFloatArgument(g,arg3,LispTrue);
  DoubleFloatArgument(g,arg4,LispTrue);
  DoubleFloatArgument(g,arg5,LispTrue);
  DoubleFloatArgument(g,arg6,LispTrue);
  g.Finalize(6);

  /* Call the actual function. */
 glFrustum(arg1, arg2, arg3, arg4, arg5, arg6);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glViewport(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  ShortIntegerArgument(g, arg2, LispTrue);
  ShortIntegerArgument(g, arg3, LispTrue);
  ShortIntegerArgument(g, arg4, LispTrue);
  g.Finalize(4);

  /* Call the actual function. */
 glViewport(arg1, arg2, arg3, arg4);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_GlutMainLoop(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  InpStringArgument(g,arg1,LispTrue);
  ShortIntegerArgument(g, arg2, LispTrue);
  ShortIntegerArgument(g, arg3, LispTrue);
  g.Finalize(3);

  /* Call the actual function. */
 GlutMainLoop(arg1, arg2, arg3);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_GlutViewWidth(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
int r =  GlutViewWidth();

/* Return result. */
  ReturnShortInteger(aEnvironment,aResult,r);
}

static void base_GlutViewHeight(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
int r =  GlutViewHeight();

/* Return result. */
  ReturnShortInteger(aEnvironment,aResult,r);
}

static void base_GlutEnableAnimation(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 GlutEnableAnimation();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_GlutDisableAnimation(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 GlutDisableAnimation();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_glutPostRedisplay(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
 glutPostRedisplay();

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_GlutGetElapsedTime(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  g.Finalize(0);

  /* Call the actual function. */
int r =  GlutGetElapsedTime();

/* Return result. */
  ReturnShortInteger(aEnvironment,aResult,r);
}
 


class ThisPlugin : public LispPluginBase
{
public:
    virtual void Add(LispEnvironment& aEnvironment);
    virtual void Remove(LispEnvironment& aEnvironment);
};
void ThisPlugin::Add(LispEnvironment& aEnvironment)
{
  SetShortIntegerConstant(aEnvironment, "GL_FALSE", GL_FALSE);
  SetShortIntegerConstant(aEnvironment, "GL_TRUE", GL_TRUE);
  SetShortIntegerConstant(aEnvironment, "GL_LINES", GL_LINES);
  SetShortIntegerConstant(aEnvironment, "GL_POINTS", GL_POINTS);
  SetShortIntegerConstant(aEnvironment, "GL_LINE_STRIP", GL_LINE_STRIP);
  SetShortIntegerConstant(aEnvironment, "GL_LINE_LOOP", GL_LINE_LOOP);
  SetShortIntegerConstant(aEnvironment, "GL_TRIANGLES", GL_TRIANGLES);
  SetShortIntegerConstant(aEnvironment, "GL_TRIANGLE_STRIP", GL_TRIANGLE_STRIP);
  SetShortIntegerConstant(aEnvironment, "GL_TRIANGLE_FAN", GL_TRIANGLE_FAN);
  SetShortIntegerConstant(aEnvironment, "GL_QUADS", GL_QUADS);
  SetShortIntegerConstant(aEnvironment, "GL_QUAD_STRIP", GL_QUAD_STRIP);
  SetShortIntegerConstant(aEnvironment, "GL_POLYGON", GL_POLYGON);
  SetShortIntegerConstant(aEnvironment, "GL_EDGE_FLAG", GL_EDGE_FLAG);
  SetShortIntegerConstant(aEnvironment, "GL_CURRENT_BIT", GL_CURRENT_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_POINT_BIT", GL_POINT_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_LINE_BIT", GL_LINE_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_POLYGON_BIT", GL_POLYGON_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_POLYGON_STIPPLE_BIT", GL_POLYGON_STIPPLE_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_PIXEL_MODE_BIT", GL_PIXEL_MODE_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_LIGHTING_BIT", GL_LIGHTING_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_FOG_BIT", GL_FOG_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_DEPTH_BUFFER_BIT", GL_DEPTH_BUFFER_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_ACCUM_BUFFER_BIT", GL_ACCUM_BUFFER_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_STENCIL_BUFFER_BIT", GL_STENCIL_BUFFER_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_VIEWPORT_BIT", GL_VIEWPORT_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_TRANSFORM_BIT", GL_TRANSFORM_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_ENABLE_BIT", GL_ENABLE_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_COLOR_BUFFER_BIT", GL_COLOR_BUFFER_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_HINT_BIT", GL_HINT_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_EVAL_BIT", GL_EVAL_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_LIST_BIT", GL_LIST_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_TEXTURE_BIT", GL_TEXTURE_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_SCISSOR_BIT", GL_SCISSOR_BIT);
  SetShortIntegerConstant(aEnvironment, "GL_ALL_ATTRIB_BITS", GL_ALL_ATTRIB_BITS);
  SetShortIntegerConstant(aEnvironment, "GL_MATRIX_MODE", GL_MATRIX_MODE);
  SetShortIntegerConstant(aEnvironment, "GL_MODELVIEW", GL_MODELVIEW);
  SetShortIntegerConstant(aEnvironment, "GL_PROJECTION", GL_PROJECTION);
  SetShortIntegerConstant(aEnvironment, "GL_TEXTURE", GL_TEXTURE);
  SetShortIntegerConstant(aEnvironment, "GLUT_LEFT_BUTTON", GLUT_LEFT_BUTTON);
  SetShortIntegerConstant(aEnvironment, "GLUT_MIDDLE_BUTTON", GLUT_MIDDLE_BUTTON);
  SetShortIntegerConstant(aEnvironment, "GLUT_RIGHT_BUTTON", GLUT_RIGHT_BUTTON);
  SetShortIntegerConstant(aEnvironment, "GLUT_DOWN", GLUT_DOWN);
  SetShortIntegerConstant(aEnvironment, "GLUT_UP", GLUT_UP);

  aEnvironment.SetCommand(base_glBegin, "glBegin");
  aEnvironment.SetCommand(base_glVertex3d, "glVertex3d");
  aEnvironment.SetCommand(base_glEnd, "glEnd");
  aEnvironment.SetCommand(base_glClearColor, "glClearColor");
  aEnvironment.SetCommand(base_glClear, "glClear");
  aEnvironment.SetCommand(base_glColor4d, "glColor4d");
  aEnvironment.SetCommand(base_glFrontFace, "glFrontFace");
  aEnvironment.SetCommand(base_glPointSize, "glPointSize");
  aEnvironment.SetCommand(base_glLineWidth, "glLineWidth");
  aEnvironment.SetCommand(base_glEnable, "glEnable");
  aEnvironment.SetCommand(base_glDisable, "glDisable");
  aEnvironment.SetCommand(base_glPushMatrix, "glPushMatrix");
  aEnvironment.SetCommand(base_glPopMatrix, "glPopMatrix");
  aEnvironment.SetCommand(base_glLoadIdentity, "glLoadIdentity");
  aEnvironment.SetCommand(base_glRotated, "glRotated");
  aEnvironment.SetCommand(base_glScaled, "glScaled");
  aEnvironment.SetCommand(base_glTranslated, "glTranslated");
  aEnvironment.SetCommand(base_glNormal3d, "glNormal3d");
  aEnvironment.SetCommand(base_glMatrixMode, "glMatrixMode");
  aEnvironment.SetCommand(base_glOrtho, "glOrtho");
  aEnvironment.SetCommand(base_glFrustum, "glFrustum");
  aEnvironment.SetCommand(base_glViewport, "glViewport");
  aEnvironment.SetCommand(base_GlutMainLoop, "GlutMainLoop");
  aEnvironment.SetCommand(base_GlutViewWidth, "GlutViewWidth");
  aEnvironment.SetCommand(base_GlutViewHeight, "GlutViewHeight");
  aEnvironment.SetCommand(base_GlutEnableAnimation, "GlutEnableAnimation");
  aEnvironment.SetCommand(base_GlutDisableAnimation, "GlutDisableAnimation");
  aEnvironment.SetCommand(base_glutPostRedisplay, "glutPostRedisplay");
  aEnvironment.SetCommand(base_GlutGetElapsedTime, "GlutGetElapsedTime");
  GlutSetEnv(aEnvironment);
  }

void ThisPlugin::Remove(LispEnvironment& aEnvironment)
{
//printf("CLOSED DLL!!!\n");
  aEnvironment.RemoveCommand("glBegin");
  aEnvironment.RemoveCommand("glVertex3d");
  aEnvironment.RemoveCommand("glEnd");
  aEnvironment.RemoveCommand("glClearColor");
  aEnvironment.RemoveCommand("glClear");
  aEnvironment.RemoveCommand("glColor4d");
  aEnvironment.RemoveCommand("glFrontFace");
  aEnvironment.RemoveCommand("glPointSize");
  aEnvironment.RemoveCommand("glLineWidth");
  aEnvironment.RemoveCommand("glEnable");
  aEnvironment.RemoveCommand("glDisable");
  aEnvironment.RemoveCommand("glPushMatrix");
  aEnvironment.RemoveCommand("glPopMatrix");
  aEnvironment.RemoveCommand("glLoadIdentity");
  aEnvironment.RemoveCommand("glRotated");
  aEnvironment.RemoveCommand("glScaled");
  aEnvironment.RemoveCommand("glTranslated");
  aEnvironment.RemoveCommand("glNormal3d");
  aEnvironment.RemoveCommand("glMatrixMode");
  aEnvironment.RemoveCommand("glOrtho");
  aEnvironment.RemoveCommand("glFrustum");
  aEnvironment.RemoveCommand("glViewport");
  aEnvironment.RemoveCommand("GlutMainLoop");
  aEnvironment.RemoveCommand("GlutViewWidth");
  aEnvironment.RemoveCommand("GlutViewHeight");
  aEnvironment.RemoveCommand("GlutEnableAnimation");
  aEnvironment.RemoveCommand("GlutDisableAnimation");
  aEnvironment.RemoveCommand("glutPostRedisplay");
  aEnvironment.RemoveCommand("GlutGetElapsedTime");
}

extern "C" {
LispPluginBase* maker(void)
{
    return new ThisPlugin;
}

};

