
/** \file lispplugin.h defines the api the plugin should provide.
 * It is a pure abstract class that needs to be rederived for each
 * platform the core needs to run on. If no dynamic loaded libraries
 * are supported on the platform, you can use LispDllBase as the default
 * DLL loader. Its basic behaviour is to fail and return NULL pointers.
 * The choice of which LispDllBase-derived class to use is made in
 * the lisptype.h file.
 *
 * Yacas has a module 'cstubgen' that can generate the plugin glue
 * code between an external library and yacas. See the manual for more
 * details.
 */

#ifndef __lispplugin_h__
#define __lispplugin_h__

/** The class that resides on the loaders side. This class
 *  is responsible for obtaining a LispPluginBase-derived
 *  class from a dll, and unloading the dll when necessary.
 *
 */
#include "yacasbase.h"

class LispPluginBase : public YacasBase
{
public:
    /**
     *  The Add method receives a LispEnvironment as argument, and
     *  can modify this environment accordingly. Things it can do is
     *  add new function calls defined in the plugin, define global
     *  variables (constants for the plugin).
     */
    virtual void Add(LispEnvironment& aEnvironment) = 0;
    virtual void Remove(LispEnvironment& aEnvironment) = 0;
    virtual ~LispPluginBase(){};
};

/** \class LispDllBase defines the features a class that can load a dll
 *  needs to have. If it fails to open or get a plugin it should return
 *  a NULL pointer. The destructor should close the DLL again.
 */
class LispDllBase : public YacasBase
{
public:
    LispDllBase();
    virtual LispInt Open(LispCharPtr aDllFile,LispEnvironment& aEnvironment);
    virtual LispInt Close(LispEnvironment& aEnvironment);
    LispPluginBase* Plugin(void);
    virtual ~LispDllBase();
    LispCharPtr DllFileName() const;
protected:
    virtual LispPluginBase* GetPlugin(LispCharPtr aDllFile);
protected:
    LispPluginBase* iPlugin;
    LispString iDllFileName;
};

#endif

