
#ifndef __glutcode_h__
#define __glutcode_h__

void GlutSetEnv(LispEnvironment& aEnv);
void GlutMainLoop(char* title, int initWidth, int initHeight);

int GlutViewWidth(void);
int GlutViewHeight(void);
void GlutEnableAnimation(void);
void GlutDisableAnimation(void);

int GlutGetElapsedTime(void);

#endif

