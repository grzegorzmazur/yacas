

#include "cyacas.h"
#include "yacas.h"

static CYacas *yacas = NULL;

void yacas_init()
{
    yacas = CYacas::NewL();
    yacas->Evaluate("DefaultDirectory(\"" SCRIPT_DIR "\");");
    yacas->Evaluate("Load(\"yacasinit.ys\");");

}
void yacas_eval(char* expression)
{
    if (yacas)
    {
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
void yacas_exit()
{
    delete yacas;
    yacas = NULL;
}


