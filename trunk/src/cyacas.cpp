

#include "cyacas.h"
#include "yacas.h"

static CYacas *yacas = NULL;

StringOutput output;
void yacas_init()
{
    yacas = CYacas::NewL(&output);
    yacas->Evaluate("DefaultDirectory(\"" SCRIPT_DIR "\");");
    yacas->Evaluate("Load(\"yacasinit.ys\");");

}
void yacas_eval(char* expression)
{
    if (yacas)
    {
        output.SetLength(0);
        output.Append('\0');
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


