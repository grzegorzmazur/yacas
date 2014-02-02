#ifndef YACAS_GENERICOBJECT_H
#define YACAS_GENERICOBJECT_H

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
  virtual bool Compare(LispInt aIndex, LispChar * aString)=0;
};

/// Abstract class which can be put inside a LispGenericClass.
class GenericClass : public YacasBase
{
public:
    GenericClass() : iReferenceCount(0) {};
    virtual ~GenericClass();
    virtual const LispChar * Send(LispArgList& aArgList)=0;
    virtual const LispChar * TypeName()=0;
public:
    ReferenceType iReferenceCount; //TODO: perhaps share the method of reference counting with how it is done in other places
};

#endif

