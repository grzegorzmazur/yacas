

#include "cyacas.h"
#include "lispstring.h"
#include "stringio.h"
#include "yacas.h"

static CYacas *yacas = NULL;
LispString *stringout = NULL;
StringOutput *output = NULL;
void yacas_init()
{
    stringout = new LispString();
    output = new StringOutput(*stringout);
    yacas = CYacas::NewL(output);
    yacas->Evaluate("DefaultDirectory(\"" SCRIPT_DIR "\");");
    yacas->Evaluate("Load(\"yacasinit.ys\");");

}
void yacas_eval(char* expression)
{
    if (yacas)
    {
        stringout->SetNrItems(0);
        stringout->Append('\0');
        yacas->Evaluate(expression);
    }
    else
    {
//        printf("ERROR: yacas has not been initialized yet!\n");
    }
}
char* yacas_error()
{
    if (yacas)
        if (yacas->IsError())
            return yacas->Error();
    return NULL;
}
char* yacas_result()
{
    if (yacas)
        return yacas->Result();
    return NULL;
}
char* yacas_output()
{
    if (yacas)
        if (stringout)
            return stringout->String();
    return NULL;
}

void yacas_interrupt()
{
  if (yacas)
    (*yacas)()().iEvalDepth = (*yacas)()().iMaxEvalDepth+100;
}

void yacas_exit()
{
    delete yacas;
    yacas = NULL;
}


