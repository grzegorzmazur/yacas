
#ifdef WIN32
//#include <afxwin.h>
#include <windows.h>
//AfxGetModuleState()->m_hCurrentResourceHandle
#endif

#include "cyacas.h"
#include "lispstring.h"
#include "stringio.h"
#include "yacas.h"
#include "archiver.h"
#include "standard.h"

static CYacas *yacas = NULL;
LispString *stringout = NULL;
StringOutput *output = NULL;

#ifdef WIN32
HANDLE hResInfo, hRes; 
#endif

void yacas_init()
{
    stringout = NEW LispString();
    output = NEW StringOutput(*stringout);
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

void yacas_secure()
{
  if (yacas)
    (*yacas)()().iSecure = LispTrue;
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












// Creating objects
void* yacas_create_atom(char* atom)
{
  return LispAtom::New((*yacas)()(),atom);
}
void* yacas_create_string(char* string)
{
  LispString orig(string);
  LispString stringified;
  InternalStringify(stringified, &orig);
  return LispAtom::New((*yacas)()(),stringified.String());
}
void* yacas_create_number_from_string(char* string)
{
  return LispAtom::New((*yacas)()(),string);
}
void* yacas_create_number_from_long(long number)
{
  BigNumber *z = new BigNumber();
  z->SetTo(number);
  return NEW LispNumber(z);
}
void* yacas_create_number_from_double(double number)
{
  BigNumber *z = new BigNumber();
  z->SetTo(number);
  return NEW LispNumber(z);
}
void* yacas_create_sublist(void* object)
{
  return LispSubList::New((LispObject*)object);
}

// Linking linked lists
void* yacas_link_objects(void* head, void* tail)
{
  LispObject *h = (LispObject *)head;
  LispObject *t = (LispObject *)tail;
  h->Next().Set(t);
  return head;
}

// executing an object
void* yacas_execute(void* object)
{
  LispPtr result;
  LispPtr input;
  LispObject* oin = ((LispObject*)object);
  input.Set(oin);
  (*yacas)()().iEvaluator->Eval((*yacas)()(), result, input);

  oin->IncreaseRefCount();
  input.Set(NULL);
  oin->DecreaseRefCount();
  LispObject* oout = result.Get();
  oout->IncreaseRefCount();
  result.Set(NULL);
  oout->DecreaseRefCount();
  return oout;
}

// pulling apart an object again
void* yacas_get_sublist(void* object)
{
  return ((LispObject*)object)->SubList()->Get();
}
char* yacas_get_atom(void* object)
{
  return ((LispObject*)object)->String()->String();
}
int  yacas_object_is_string(void* object)
{
  if (((LispObject*)object)->String() == NULL)
    return 0;
  if (((LispObject*)object)->String()->String()[0] != '\"')
    return 0;
  return 1;
}
int  yacas_object_is_sublist(void* object)
{
  return (((LispObject*)object)->SubList() != NULL);
}
int  yacas_object_is_atom(void* object)
{
  return (((LispObject*)object)->String() != NULL);
}
int  yacas_object_is_number(void* object)
{
  return (((LispObject*)object)->Number(0) != NULL);
}
int  yacas_object_is_integer(void* object)
{
  return (((LispObject*)object)->Number(0)->IsInt());
}
long yacas_get_long(void* object)
{
  return (long)(((LispObject*)object)->Number(0)->Double());
}
double yacas_get_double(void* object)
{
  return (((LispObject*)object)->Number(0)->Double());
}

void* yacas_get_next(void* object)
{
  return (((LispObject*)object)->Next().Get());
}
void yacas_delete_object(void* object)
{
  delete ((LispObject*)object);
}

