#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
CORE_KERNEL_FUNCTION("MathAdd",LispAdd,2,YacasEvaluator::Function | YacasEvaluator::Fixed)

    aEnvironment.CoreCommands().SetAssociation(
         new YacasEvaluator(new LispAdd(),2, YacasEvaluator.Fixed|YacasEvaluator.Function),
         "MathAdd");

  class LispAdd extends YacasEvalCaller
  {
    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Yacasexception
    {
      //TODO FIXME
    }
  }



#define CORE_KERNEL_FUNCTION_ALIAS(iname,fname,nrargs,flags) iEnvironment.SetCommand(fname,iname,nrargs,flags);

*/

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
"    public void Eval(LispEnvironment aEnvironment,int aStackTop) throws Yacasexception\n"
"    {\n"
"      //TODO FIXME\n"
"    }\n"
"  }\n\n",fname
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
#include "corefunctions.txt"
  fclose(fproto);
  fclose(fclass);
  return 0;
}
