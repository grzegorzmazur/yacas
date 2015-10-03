#ifndef YACAS_GENERICOBJECT_H
#define YACAS_GENERICOBJECT_H

#include "yacasbase.h"

/// Abstract class which can be put inside a LispGenericClass.
class GenericClass : public YacasBase
{
public:
    GenericClass() : iReferenceCount(0) {};
    virtual ~GenericClass() = default;
    virtual const LispChar* TypeName() const = 0;
public:
    ReferenceType iReferenceCount; //TODO: perhaps share the method of reference counting with how it is done in other places
};

#endif

