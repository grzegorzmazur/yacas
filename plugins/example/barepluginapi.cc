
/* This file was automatically generated with cstubgen.
*/
#include "lisptype.h"
#include "lispenvironment.h"
#include "lispatom.h"
#include "standard.h"
#include "arggetter.h"
#include "lispplugin.h"
#include "platmath.h"
#include "stubs.h"
#include "genericstructs.h"

#include "bareplugin.h"


static void base_add_integers(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  ShortIntegerArgument(g, arg2, LispTrue);
  g.Finalize(2);

  /* Call the actual function. */
int r =  add_integers(arg1, arg2);

/* Return result. */
  ReturnShortInteger(aEnvironment,aResult,r);
}

static void base_add_doubles(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  DoubleFloatArgument(g,arg1,LispTrue);
  DoubleFloatArgument(g,arg2,LispTrue);
  g.Finalize(2);

  /* Call the actual function. */
double r =  add_doubles(arg1, arg2);

/* Return result. */
  ReturnDoubleFloat(aEnvironment,aResult,r);
}

static void base_CreateBla(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  ShortIntegerArgument(g, arg1, LispTrue);
  ShortIntegerArgument(g, arg2, LispTrue);
  g.Finalize(2);

  /* Call the actual function. */
void* r =  CreateBla(arg1, arg2);

/* Return result. */
  ReturnVoidStruct(aEnvironment, aResult,"Bla*", r,Bla_free);
}

static void base_BlaSetA(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  VoidStructArgument(Bla*,g,arg1,LispTrue,"Bla*");
  ShortIntegerArgument(g, arg2, LispTrue);
  g.Finalize(2);

  /* Call the actual function. */
 BlaSetA(arg1, arg2);

/* Return result. */
  InternalTrue(aEnvironment,aResult);
}

static void base_BlaGetA(LispEnvironment& aEnvironment, LispPtr& aResult,
                           LispPtr& aArguments)
{
  /* Obtain arguments passed in. */
  LispArgGetter g(aEnvironment, aArguments);
  VoidStructArgument(Bla*,g,arg1,LispTrue,"Bla*");
  g.Finalize(1);

  /* Call the actual function. */
int r =  BlaGetA(arg1);

/* Return result. */
  ReturnShortInteger(aEnvironment,aResult,r);
}
 


class ThisPlugin : public LispPluginBase
{
public:
    virtual void Add(LispEnvironment& aEnvironment);
};
void ThisPlugin::Add(LispEnvironment& aEnvironment)
{

  aEnvironment.SetCommand(base_add_integers, "AddTwoIntegers");
  aEnvironment.SetCommand(base_add_doubles, "AddTwoDoubles");
  aEnvironment.SetCommand(base_CreateBla, "CreateBla");
  aEnvironment.SetCommand(base_BlaSetA, "BlaSetA");
  aEnvironment.SetCommand(base_BlaGetA, "BlaGetA");
}


extern "C" {
LispPluginBase* maker(void)
{
    return new ThisPlugin;
}

};

