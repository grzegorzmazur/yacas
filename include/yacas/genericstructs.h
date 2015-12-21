/** \file genericstructs.h defines the class that handles structs and classes for plugins.
 */
#ifndef YACAS_GENERICSTRUCT_H
#define YACAS_GENERICSTRUCT_H

#include "yacasbase.h"
#include "genericobject.h"
#include "noncopyable.h"


/** \class GenericStruct This class maintains a pointer to some arbitrary
 *  object (which can be any thing). The plugin is responsible for supplying
 *  functions for manipulating such structs/classes/arrays. The Yacas core
 *  then needs to know nothing about the internals of such a struct.
 *
 *  The struct is represented by a void pointer to the struct, a pointer
 *  to a function that can clean up the struct (used when automatically
 *  deleting the object), and a pointer to a text string representing the
 *  type of the object (useful for testing if the type passed as an argument
 *  to a function is correct).
 */
class GenericStruct : public GenericClass, NonCopyable
{
public:
  GenericStruct(LispChar * aTypeName, void* aData, void (*aDestructor)(void*));
  virtual ~GenericStruct();
  virtual const LispChar * TypeName() const override;
  inline void* Data() {return iData;}

  void* iData;
  LispChar * iTypeName;
  void (*iDestructor)(void* data);
};


#endif

