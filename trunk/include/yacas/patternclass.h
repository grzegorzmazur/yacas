#ifndef YACAS_PATTERNCLASS_H
#define YACAS_PATTERNCLASS_H

#include "lisptype.h"
#include "lispobject.h"
#include "genericobject.h"
#include "patterns.h"

/// Wrapper for YacasPatternPredicateBase.
/// This class allows a YacasPatternPredicateBase to be put in a
/// LispGenericObject.
class PatternClass : public GenericClass
{
public:
  PatternClass(YacasPatternPredicateBase* aPatternMatcher);
  ~PatternClass();

  bool Matches(LispEnvironment& aEnvironment,
                      LispPtr& aArguments);
  bool Matches(LispEnvironment& aEnvironment,
                      LispPtr* aArguments);
public: //From GenericClass
  virtual const LispChar * Send(LispArgList& aArgList);
  virtual const LispChar * TypeName();

private:
  PatternClass(const PatternClass& aOther): iPatternMatcher(NULL)
  {
    // copy constructor not written yet, hence the assert
    assert(0);
  }
  PatternClass& operator=(const PatternClass& aOther)
  {
    // copy constructor not written yet, hence the assert
    assert(0);
    return *this;
  }
protected:
  YacasPatternPredicateBase* iPatternMatcher;
};




#endif

