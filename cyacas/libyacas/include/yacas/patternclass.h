#ifndef YACAS_PATTERNCLASS_H
#define YACAS_PATTERNCLASS_H

#include "lispobject.h"
#include "genericobject.h"
#include "patterns.h"
#include "noncopyable.h"

/// Wrapper for YacasPatternPredicateBase.
/// This class allows a YacasPatternPredicateBase to be put in a
/// LispGenericObject.
class PatternClass final: public GenericClass, NonCopyable
{
public:
  PatternClass(YacasPatternPredicateBase* aPatternMatcher);
  ~PatternClass();

  bool Matches(LispEnvironment& aEnvironment,
                      LispPtr& aArguments);
  bool Matches(LispEnvironment& aEnvironment,
                      LispPtr* aArguments);

  const char* TypeName() const override;

protected:
  YacasPatternPredicateBase* iPatternMatcher;
};




#endif

