

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


/* Definition of DYNCAST: either the cast succeeds, or the variable is assigned NULL.
*/

#define HAS_NEW_GC_dynamic_cast 0	/* requires RTTI if set */
#if HAS_NEW_GC_dynamic_cast 
#define DYNCAST(_type,_name,_var,_object) _type * _var = dynamic_cast<_type *>(_object);
#else // HAS_NEW_GC_dynamic_cast
#define DYNCAST(_type,_name,_var,_object) \
    _type * _var = NULL ; \
    if (_object != NULL) \
    { \
      if (StrEqual((_object)->TypeName(),_name)) _var = (_type *)(_object); \
    }
#endif // HAS_NEW_GC_dynamic_cast


#endif

