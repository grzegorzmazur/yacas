
/* This file was automatically generated with cstubgen.
*/

#include <stdio.h> //TODO remove, debug!

#include "lisptype.h"
#include "lispenvironment.h"
#include "lispatom.h"
#include "standard.h"
#include "arggetter.h"
#include "lispplugin.h"
#include "platmath.h"
#include "stubs.h"
#include "genericstructs.h"
#include "infixparser.h"

#include "errors.h"

#include "pcre.h"
struct TPattern
{
  pcre *re;
  pcre_extra *pe;
  LispPtr type;
};
#define KMaxPatterns 256
TPattern patterns[KMaxPatterns];
int nrPatterns = 0;


void AddPattern(const char* aPattern, LispPtr& aType)
{
  const char *error;
  int erroffset;
  patterns[nrPatterns].re = pcre_compile(
    aPattern,          /* the pattern */
    PCRE_DOTALL,                /* default options */
    &error,           /* for error message */
    &erroffset,       /* for error offset */
    NULL);            /* use default character tables */

  patterns[nrPatterns].pe = pcre_study(
    patterns[nrPatterns].re,             /* result of pcre_compile() */
    0,              /* no options exist */
    &error);        /* set to NULL or points to a message */
  patterns[nrPatterns].type.Set(aType.Get());
  nrPatterns++;
}

void FreePatterns(void)
{
  int i;
  for (i=0;i<nrPatterns;i++)
  {
    free(patterns[i].pe);
    free(patterns[i].re);
    patterns[i].type.Set(NULL);
  }
  nrPatterns=0;
}


#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)

static void PcreNextToken(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  if (aEnvironment.CurrentInput()->EndOfStream())
  {
    RESULT.Set(LispAtom::New(aEnvironment,"EndOfFile"));
    return;
  }

//printf("Entered PcreNextToken\n");
    LispInt pos = aEnvironment.CurrentInput()->Position();
    LispCharPtr trav = &aEnvironment.CurrentInput()->StartPtr()[pos];

//printf("trav is %s\n",trav);

    int count = 0;
    int ovector[10];
    int i;
    for (i=0;i<nrPatterns;i++)
    {
      count = pcre_exec(
        patterns[i].re, patterns[i].pe, trav, strlen(trav), 0, 0, ovector, 10); 

      if (count == 1)
      {
        break;
      }
    }

//printf("count = %d, i = %d\n",count,i);
    if (count < 1)
    {
      RESULT.Set(LispAtom::New(aEnvironment,"EndOfFile"));
      return;
    }
    char* resultbuf = (char*)PlatAlloc(ovector[1]-ovector[0]+3); //TODO use plat allocs!
    char*trg = resultbuf;
    *trg++ = '\"';
    memcpy(trg,&trav[ovector[0]],ovector[1]-ovector[0]);
    trg[ovector[1]-ovector[0]] = '\0';
    strcat(trg,"\"");
    while (aEnvironment.CurrentInput()->Position() < pos+ovector[1]) aEnvironment.CurrentInput()->Next();

//TODO remove    aEnvironment.CurrentInput()->SetPosition(pos+ovector[1]);
    LispObject *res = NULL;
    res = LA(patterns[i].type.Get())+LA(res);
    res = LA(ATOML(resultbuf)) + LA(res);
    RESULT.Set(LIST(LA(ATOML("List")) + LA(res)));
    PlatFree(resultbuf); //TODO use plat allocs!
}

static void PcreLexer(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  FreePatterns();
    LispPtr list;
    list.Set(ARGUMENT(1).Get());

    LispObject* t;

    //Check that it is a compound object
    CHK_ARG_CORE(list.Get()->SubList() != NULL, 1);
    t = list.Get()->SubList()->Get();
    CHK_ARG_CORE(t != NULL, 2);
    t = t->Next().Get();
    while (t != NULL)
    {
        if (t->SubList())
        {
            LispObject* sub = t->SubList()->Get();
            if (sub)
            {
                sub = sub->Next().Get();
                if(sub == NULL) 
                  RaiseError("Invalid argument in PcreLexer: not enough elements in a sublist");
                LispStringPtr pattern = aEnvironment.HashTable().LookUpUnStringify(sub->String()->String());
                LispPtr type;
                if(sub->Next().Get() == NULL) 
                  RaiseError("Invalid argument in PcreLexer: not enough elements in a sublist");
                type.Set(sub->Next().Get()->Copy(LispFalse));

//printf("Pattern \"%s\" type %d\n",pattern->String(),type);
                AddPattern(pattern->String(),type);
            }
        }
        t = t->Next().Get();
    }

  InternalTrue(aEnvironment, RESULT);
}


class PcrePlugin : public LispPluginBase
{
public:
    virtual void Add(LispEnvironment& aEnvironment);
    virtual void Remove(LispEnvironment& aEnvironment);
};
void PcrePlugin::Add(LispEnvironment& aEnvironment)
{
  aEnvironment.SetCommand(PcreLexer, "PcreLexer",1,YacasEvaluator::Function | YacasEvaluator::Fixed);
  aEnvironment.SetCommand(PcreNextToken, "PcreNextToken",0,YacasEvaluator::Function | YacasEvaluator::Fixed);
}

void PcrePlugin::Remove(LispEnvironment& aEnvironment)
{
  aEnvironment.RemoveCoreCommand("PcreLexer");
  aEnvironment.RemoveCoreCommand("PcreNextToken");
}


extern "C" {

#ifdef EXE_DLL_PLUGINS
LispPluginBase* make_pcre(void)
#else
LispPluginBase* maker(void)
#endif
{
    return NEW PcrePlugin;
}

};





