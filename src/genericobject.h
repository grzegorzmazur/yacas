

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
  virtual LispCharPtr GetArgument(LispInt aIndex, LispInt& aLength)=0;
  virtual LispBoolean Compare(LispInt aIndex, LispCharPtr aString)=0;
};

/// Abstract class which can be put inside a LispGenericClass.
class GenericClass : public YacasBase
{
public:
    GenericClass() : iReferenceCount(0) {};
    virtual ~GenericClass();
    virtual LispCharPtr Send(LispArgList& aArgList)=0;
    virtual LispCharPtr TypeName()=0;
public:
    LispInt iReferenceCount;
};

#endif

