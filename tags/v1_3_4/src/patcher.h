#ifndef YACAS_PATCHER_H
#define YACAS_PATCHER_H

void PatchLoad(const LispChar* aFileContent, LispOutput& aOutput,
               LispEnvironment& aEnvironment);

#endif

