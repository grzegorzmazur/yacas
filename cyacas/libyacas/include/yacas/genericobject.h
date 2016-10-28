#ifndef YACAS_GENERICOBJECT_H
#define YACAS_GENERICOBJECT_H

/// Abstract class which can be put inside a LispGenericClass.
class GenericClass {
public:
    GenericClass() : iReferenceCount(0) {};
    virtual ~GenericClass() = default;
    virtual const char* TypeName() const = 0;
public:
    unsigned iReferenceCount; //TODO: perhaps share the method of reference counting with how it is done in other places
};

#endif

