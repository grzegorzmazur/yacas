#ifndef YACAS_LISPUSERFUNC_H
#define YACAS_LISPUSERFUNC_H

#include "lispobject.h"
#include "evalfunc.h"

#include <vector>
#include <unordered_map>

class LispEnvironment;

/// Abstract class providing the basic user function API.
/// Instances of this class are associated to the name of the function
/// via an associated hash table. When obtained, they can be used to
/// evaluate the function with some arguments.

class LispUserFunction : public EvalFuncBase
{
public:
    LispUserFunction() : iFenced(true),iTraced(false) {};
    virtual void HoldArgument(const LispString* aVariable) = 0;
    virtual void DeclareRule(int aPrecedence, LispPtr& aPredicate,
                             LispPtr& aBody) = 0;
    virtual void DeclareRule(int aPrecedence, LispPtr& aBody) = 0;
    virtual void DeclarePattern(int aPrecedence, LispPtr& aPredicate,
                             LispPtr& aBody) = 0;
    virtual const LispPtr& ArgList() const = 0;

public: //unfencing
    inline void UnFence() {iFenced = false;};
    inline bool Fenced() const {return iFenced;};
public: //tracing
    inline void Trace() {iTraced = true;};
    inline void UnTrace() {iTraced = false;};
    inline bool Traced() const {return iTraced;};
private:
    bool iFenced;
    bool iTraced;
};


/// User function with a specific arity.
/// This is still an abstract class, but the arity (number of
/// arguments) of the function is now fixed.

class LispArityUserFunction : public LispUserFunction
{
public:
    virtual int Arity() const = 0;
    virtual int IsArity(int aArity) const = 0;
};


class LispDefFile;


/// Set of LispArityUserFunction's.
/// By using this class, you can associate multiple functions (with
/// different arities) to one name. A specific LispArityUserFunction
/// can be selected by providing its name. Additionally, the name of
/// the file in which the function is defined, can be specified.

class LispMultiUserFunction final {
public:
  /// Constructor.
  LispMultiUserFunction() : iFunctions(),iFileToOpen(nullptr) {};

  /** When adding a multi-user function to the association hash table, the copy constructor is used.
   *  We should at least make sure that iFunctions is empty, so there is no copying needed (for efficiency).
   *  Casually having a copy-constructor on CDeletingArrayGrower should be avoided, to make sure it is
   *  not used accidentally.
   */
  inline LispMultiUserFunction(const LispMultiUserFunction& aOther) : iFunctions(), iFileToOpen(nullptr)
  {
    assert(aOther.iFileToOpen == 0);
    assert(aOther.iFunctions.size() == 0);
  }
  inline LispMultiUserFunction& operator=(const LispMultiUserFunction& aOther)
  {
    // copy constructor not written yet, hence the assert
    assert(aOther.iFileToOpen == 0);
    assert(aOther.iFunctions.size() == 0);

    assert(iFileToOpen == 0);
    assert(iFunctions.size() == 0);
    return *this;
  }

  /// Return user function with given arity.
  LispUserFunction* UserFunc(int aArity);

  /// Destructor.
  virtual ~LispMultiUserFunction();

  /// Specify that some argument should be held.
  virtual void HoldArgument(const LispString* aVariable);

  /// Add another LispArityUserFunction to #iFunctions.
  virtual void DefineRuleBase(LispArityUserFunction* aNewFunction);

  /// Delete tuser function with given arity.
  virtual void DeleteBase(int aArity);

private:
  /// Set of LispArityUserFunction's provided by this LispMultiUserFunction.
  std::vector<LispArityUserFunction*> iFunctions;

public:
  /// File to read for the definition of this function.
  LispDefFile* iFileToOpen;
};


/// Associated hash of LispMultiUserFunction objects.
typedef std::unordered_map<LispStringSmartPtr, LispMultiUserFunction, std::hash<const LispString*> > LispUserFunctions;


#endif

