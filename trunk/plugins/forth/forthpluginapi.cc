
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

#include "forthplugin.h"

 


class ThisPlugin : public LispPluginBase
{
public:
    virtual void Add(LispEnvironment& aEnvironment);
};
void ThisPlugin::Add(LispEnvironment& aEnvironment)
{

}


extern "C" {
LispPluginBase* maker(void)
{
    return new ThisPlugin;
}

};

