
#ifdef WIN32
//#include <afxwin.h>
#include <windows.h>
//AfxGetModuleState()->m_hCurrentResourceHandle
#endif

#include "cyacas.h"
#include "lispstring.h"
#include "stringio.h"
#include "yacas.h"

static CYacas *yacas = NULL;
LispString *stringout = NULL;
StringOutput *output = NULL;

#ifdef WIN32
HANDLE hResInfo, hRes; 
#endif

void yacas_init()
{
    stringout = new LispString();
    output = new StringOutput(*stringout);
    yacas = CYacas::NewL(output);
#ifdef WIN32

/* */
  { 
    // Find the wave resource.
    extern HANDLE hThisModule;
    HMODULE hmod = (HMODULE)hThisModule;
    hResInfo = FindResource (hmod, "RC_DATA1", RT_RCDATA); 

    if (hResInfo == NULL)
    {
      printf("Error %d\n",GetLastError());
    }

    int fullsize = 0;
    if (hResInfo != NULL) 
    {
      fullsize = SizeofResource(hmod, (HRSRC)hResInfo);
      // Load the wave resource. 
      hRes = LoadResource (hmod, (HRSRC)hResInfo); 

      if (hRes != NULL) 
      {
        unsigned char* buffer;
        // Lock the wave resource and play it. 
        buffer = (unsigned char*)PlatAlloc(fullsize);
     
        if (buffer)
        {
          memcpy(buffer, (unsigned char*)LockResource (hRes),fullsize);
          CCompressedArchive *a =
              NEW CCompressedArchive(buffer, fullsize, 1);
          (*yacas)()().iArchive = a;
        }
      }
    }
  }
/* */
#else
    yacas->Evaluate("DefaultDirectory(\"" SCRIPT_DIR "\");");
#endif
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


