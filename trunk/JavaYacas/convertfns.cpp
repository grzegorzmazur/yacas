#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void doit(FILE* fproto,FILE* fclass,char* iname,char* fname,int nrargs,char* flags)
{
//Fixed, Variable, Function, Macro
  char flag_str[256];
  flag_str[0] = '\0';
  if (strstr(flags,"Fixed"))
  {
    if (flag_str[0]) strcat(flag_str,"|");
    strcat(flag_str,"YacasEvaluator.Fixed");
  }
  if (strstr(flags,"Variable"))
  {
    if (flag_str[0]) strcat(flag_str,"|");
    strcat(flag_str,"YacasEvaluator.Variable");
  }
  if (strstr(flags,"Function"))
  {
    if (flag_str[0]) strcat(flag_str,"|");
    strcat(flag_str,"YacasEvaluator.Function");
  }
  if (strstr(flags,"Macro"))
  {
    if (flag_str[0]) strcat(flag_str,"|");
    strcat(flag_str,"YacasEvaluator.Macro");
  }


  fprintf(fproto,
"    aEnvironment.CoreCommands().SetAssociation(\n"
"         new YacasEvaluator(new %s(),%d, %s),\n"
"         \"%s\");\n",fname,nrargs,flag_str,iname
);
  fprintf(fclass,
"  class %s extends YacasEvalCaller\n"
"  {\n"
"    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Exception\n"
"    {\n"
"      aEnvironment.iCurrentOutput.Write(\"Function not yet implemented : %s\");\n"
"    }\n"
"  }\n\n",fname,fname
);
}

void doit_alias(FILE* fproto,FILE* fclass,char* iname,char* fname,int nrargs,char* flags)
{
  char flag_str[256];
  flag_str[0] = '\0';
  if (strstr(flags,"Fixed"))
  {
    if (flag_str[0]) strcat(flag_str,"|");
    strcat(flag_str,"YacasEvaluator.Fixed");
  }
  if (strstr(flags,"Variable"))
  {
    if (flag_str[0]) strcat(flag_str,"|");
    strcat(flag_str,"YacasEvaluator.Variable");
  }
  if (strstr(flags,"Function"))
  {
    if (flag_str[0]) strcat(flag_str,"|");
    strcat(flag_str,"YacasEvaluator.Function");
  }
  if (strstr(flags,"Macro"))
  {
    if (flag_str[0]) strcat(flag_str,"|");
    strcat(flag_str,"YacasEvaluator.Macro");
  }
  fprintf(fproto,
"    aEnvironment.CoreCommands().SetAssociation(\n"
"         new YacasEvaluator(new %s(),%d, %s),\n"
"         \"%s\");\n",fname,nrargs,flag_str,iname
);
}

int main(int argc, char** argv)
{
  FILE* fproto = fopen("tempproto.tmp","wb");
  FILE* fclass = fopen("tempclass.tmp","wb");
#define CORE_KERNEL_FUNCTION_ALIAS(iname,fname,nrargs,flags) doit_alias(fproto,fclass,iname,#fname,nrargs,#flags);
#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) doit(fproto,fclass,iname,#fname,nrargs,#flags);
//#include "corefunctions.txt"

CORE_KERNEL_FUNCTION("OSVersion",LispPlatformOS,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("Exit",LispExit,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("IsExitRequested",LispExitRequested,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("HistorySize",LispHistorySize,1,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("StaSiz",LispStackSize,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("IsPromptShown",LispIsPromptShown,0,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("ReadCmdLineString",LispReadCmdLineString,1,YacasEvaluator::Function | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("GetTime",LispTime,1,YacasEvaluator::Macro | YacasEvaluator::Fixed)
CORE_KERNEL_FUNCTION("FileSize",LispFileSize,1,YacasEvaluator::Function | YacasEvaluator::Fixed)


  fclose(fproto);
  fclose(fclass);
  return 0;
}
