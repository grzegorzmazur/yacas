
#ifndef __arggetter_h__
#define __arggetter_h__

#include "yacasbase.h"


/**
 LispArgGetter: a class that at construction gets the arguments, and
 the function calling it just loops on until the end, getting arguments
 by type.
 */


class LispArgGetter : public YacasBase
{
public:
    inline LispArgGetter(LispEnvironment& aEnvironment, LispPtr& aArguments);
public:
    /// Get an argument that should be a (long) integer
    LispStringPtr GetIntegerArgument(LispInt aEvaluate);
    /// Get a string (atom)
    LispStringPtr GetStringArgument(LispInt aEvaluate);
    /// Get the atomic string of the argument
    LispStringPtr GetAtomArgument(LispInt aEvaluate);
    /// Get an argument that should be a short integer
    LispInt GetShortIntegerArgument(LispInt aEvaluate);
    /// Get a list argument
    void GetListArgument(LispPtr& aResult, LispInt aEvaluate);
    /// Get a void* pointer to a struct encapsulated in a generic class
    void* GetVoidStruct(LispInt aEvaluate, LispCharPtr aTypeString);
    /// Check that we are at the end of the list.
    void Finalize(LispInt aNrArgsNeeded);
private:
    void GoNext();
private:
    LispEnvironment& iEnvironment;
    LispPtr& iArguments;
    LispIterator iIter;
    LispInt iNrArgsParsed;
    LispInt iNrArgsNeeded;
};

inline LispArgGetter::LispArgGetter(LispEnvironment& aEnvironment, LispPtr& aArguments)
: iEnvironment(aEnvironment),iArguments(aArguments), iIter(Argument(aArguments,0)),iNrArgsParsed(0)
{}

#define ListArgument(_g,_list,_bool) LispPtr _list; (_g).GetListArgument(_list, _bool)
#define IntegerArgument(_g,_i,_bool) LispStringPtr _i = (_g).GetIntegerArgument(_bool)
#define ShortIntegerArgument(_g,_i,_bool) LispInt _i = (_g).GetShortIntegerArgument(_bool)
#define InpStringArgument(_g,_i,_bool) LispCharPtr _i = (_g).GetStringArgument(_bool)->String()

#define DoubleFloatArgument(_g,_i,_bool) double _i = GetDoubleFloatArgument(_g,_bool)
#define VoidStructArgument(_typ,_g,_i,_bool,_name) _typ _i = (_typ)(_g).GetVoidStruct(_bool,_name)


void ReturnShortInteger(LispEnvironment& aEnvironment,
                        LispPtr& aResult, LispInt r);
void SetShortIntegerConstant(LispEnvironment& aEnvironment,
                                    LispCharPtr aName,
                                    LispInt aValue);
double GetDoubleFloatArgument(LispArgGetter& g,LispInt aEvaluate);
void ReturnDoubleFloat(LispEnvironment& aEnvironment,
                       LispPtr& aResult, double r);


void ReturnVoidStruct(LispEnvironment& aEnvironment,
                      LispPtr& aResult,
                      LispCharPtr aName,
                      void* aData,
                      void (*aFree)(void*));


#endif

