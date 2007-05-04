
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

    if (!hResInfo)
    {
      printf("Error %d\n",GetLastError());
    }

    int fullsize = 0;
    if (hResInfo) 
    {
      fullsize = SizeofResource(hmod, (HRSRC)hResInfo);
      // Load the wave resource. 
      hRes = LoadResource (hmod, (HRSRC)hResInfo); 

      if (hRes) 
      {
        // Lock the wave resource and play it. 
        unsigned char* buffer = PlatAllocN<unsigned char>(fullsize);
        if (buffer)
        {
          memcpy(buffer, (unsigned char*)LockResource (hRes),fullsize);
          CCompressedArchive *a =
              NEW CCompressedArchive(buffer, fullsize, 1);
          yacas->getDefEnv().getEnv().iArchive = a;
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
        stringout->ResizeTo(0);
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
            return stringout->c_str();
    return NULL;
}

void yacas_secure()
{
  if (yacas)
    yacas->getDefEnv().getEnv().iSecure = LispTrue;
}
void yacas_interrupt()
{
  if (yacas)
    yacas->getDefEnv().getEnv().iEvalDepth = yacas->getDefEnv().getEnv().iMaxEvalDepth+100;
}

void yacas_exit()
{
    delete yacas;
    yacas = NULL;
}












// Creating objects
void* yacas_create_atom(char* atom)
{
  return LispAtom::New(yacas->getDefEnv().getEnv(),atom);
}
void* yacas_create_string(char* string)
{
  LispString orig(string);
  LispString stringified;
  InternalStringify(stringified, &orig);
  return LispAtom::New(yacas->getDefEnv().getEnv(),stringified.c_str());
}
void* yacas_create_number_from_string(char* string)
{
  return LispAtom::New(yacas->getDefEnv().getEnv(),string);
}
void* yacas_create_number_from_long(long number)
{
  BigNumber *z = NEW BigNumber();
  z->SetTo(number);
  return NEW LispNumber(z);
}
void* yacas_create_number_from_double(double number)
{
  BigNumber *z = NEW BigNumber();
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
  h->Nixed() = (t);
  return head;
}

// executing an object
void* yacas_execute(void* object)
{
  LispPtr result;
  LispPtr input;
  LispObject* oin = ((LispObject*)object);
  input = (oin);
  yacas->getDefEnv().getEnv().iEvaluator->Eval(yacas->getDefEnv().getEnv(), result, input);

  ++oin->iReferenceCount;
  input = (NULL);
  --oin->iReferenceCount;
  LispObject* oout = result;
  ++oout->iReferenceCount;
  result = (NULL);
  --oout->iReferenceCount;
  return oout;
}

// pulling apart an object again
void* yacas_get_sublist(void* object)
{
  //return ((LispObject*)object)->SubList()->Get();
	return (void*)&(*(*((LispObject*)object)->SubList()));
}
char* yacas_get_atom(void* object)
{
  return ((LispObject*)object)->String()->c_str();
}
int  yacas_object_is_string(void* object)
{
  if (!((LispObject*)object)->String())
    return 0;
  if (((LispObject*)object)->String()->c_str()[0] != '\"')
    return 0;
  return 1;
}
int  yacas_object_is_sublist(void* object)
{
  return !!(((LispObject*)object)->SubList());
}
int  yacas_object_is_atom(void* object)
{
  return !!(((LispObject*)object)->String());
}
int  yacas_object_is_number(void* object)
{
  return !!(((LispObject*)object)->Number(0));
}
int  yacas_object_is_integer(void* object)
{
  BigNumber * pNum = ((LispObject*)object)->Number(0);
  return pNum ? pNum->IsInt() : 0;
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
  return (void*)&(*(((LispObject*)object)->Nixed()));
}
void yacas_delete_object(void* object)
{
  delete ((LispObject*)object);
}

