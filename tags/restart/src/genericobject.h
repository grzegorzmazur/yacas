

#ifndef __genericobject_h__
#define __genericobject_h__

#include "lisptype.h"

class LispArgList
{
public:
  virtual ~LispArgList();
  virtual LispInt NrArguments()=0;
  virtual LispCharPtr GetArgument(LispInt aIndex, LispInt& aLength)=0;
  virtual LispBoolean Compare(LispInt aIndex, LispCharPtr aString)=0;
};

class GenericClass
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

