

#ifndef __genericobject_h__
#define __genericobject_h__

#include "yacasbase.h"

/// Not used.
/// This class has pure virtual functions, but no derived classes, so
/// it can never be used.
class LispArgList : public YacasBase
{
public:
  virtual ~LispArgList();
  virtual LispInt NrArguments()=0;
  virtual LispChar * GetArgument(LispInt aIndex, LispInt& aLength)=0;
  virtual LispBoolean Compare(LispInt aIndex, LispChar * aString)=0;
};

/// Abstract class which can be put inside a LispGenericClass.
class GenericClass : public YacasBase
{
public:
    GenericClass() : iReferenceCount(0) {};
    virtual ~GenericClass();
    virtual LispChar * Send(LispArgList& aArgList)=0;
    virtual LispChar * TypeName()=0;
public:
    LispInt iReferenceCount;	// TODO: woof
};
#define HAS_NEW_GC_dynamic_cast 0	/* requires RTTI if set */

#endif

