
#include "lisptype.h"
#include "lispenvironment.h"
#include "lispatom.h"
#include "standard.h"
#include "arggetter.h"
#include "lispplugin.h"
#include "platmath.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef HAVE_CONFIG_H
#include "../../../config.h"
#endif


//#define HAVE_GLUT_H
#ifdef HAVE_GLUT_H
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

static LispEnvironment* env = NULL;
static int iViewWidthInPixels;
static int iViewHeightInPixels;
static int iNeedPerspective = 0;


void GlutSetEnv(LispEnvironment& aEnv)
{
    env = &aEnv;
}


static char sendoff[100];

void KeyHandler ( unsigned char key, int x, int y )
{
    sprintf(sendoff,"GlutKeyHandler(\"%c\",%d,%d);",key,x,y);
    LispPtr result;
    InternalEvalString(*env, result, sendoff);
}

void SpecialKey(int key, int x, int y)
{
    sprintf(sendoff,"GlutSpecialKeyHandler(\"%c\",%d,%d);",key,x,y);
    LispPtr result;
    InternalEvalString(*env, result, sendoff);
}

void MousePress(int button, int state, int x, int y)
{
    sprintf(sendoff,"GlutMousePress(%d,%d,%d,%d);", button,state,
            x,iViewHeightInPixels-y-1);
    LispPtr result;
    InternalEvalString(*env, result, sendoff);
}



void MouseDrag ( int x, int y )
{
    sprintf(sendoff,"GlutMouseDrag(%d,%d);", x,iViewHeightInPixels-y-1);
    LispPtr result;
    InternalEvalString(*env, result, sendoff);
}

int GlutViewWidth(void)
{
    return iViewWidthInPixels;
}
int GlutViewHeight(void)
{
    return iViewHeightInPixels;
}

void myReshape(int aNewWidth, int aNewHeight)
{
     if (aNewHeight == 0) return;

    iViewWidthInPixels  = aNewWidth;
    iViewHeightInPixels = aNewHeight;
    LispPtr result;
    InternalEvalString(*env, result, "GlutReshape();");
}

void myDisplay(void)
{
    LispPtr result;
    InternalEvalString(*env, result, "GlutRender();");
    glutSwapBuffers();
}


void myIdleFunc(void)
{
    LispPtr result;
    InternalEvalString(*env, result, "GlutIdle();");
}

void GlutEnableAnimation(void)
{
    glutIdleFunc(myIdleFunc);
}
void GlutDisableAnimation(void)
{
    glutIdleFunc(NULL);
}


int GlutGetElapsedTime(void)
{
    glutGet(GLUT_ELAPSED_TIME);
}




// Possible arguments:
// 1. window title, initial width, initial height.
//
void GlutMainLoop(char* title, int initWidth, int initHeight)
{
    int argc=1;
    char *argv[10] = {"hello","bbb"};
    glutInit(&argc, argv);
    glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowSize(initWidth, initHeight);

    glutCreateWindow (title);

    glutKeyboardFunc(KeyHandler);
    glutSpecialFunc(SpecialKey);
    glutMouseFunc(MousePress);
    glutMotionFunc(MouseDrag);

    glutReshapeFunc (myReshape);
    glutDisplayFunc(myDisplay);

    glutMainLoop();
}


#endif //HAVE_GLUT_H
