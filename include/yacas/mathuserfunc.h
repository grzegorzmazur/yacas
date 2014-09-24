#ifndef YACAS_MATHUSERFUNC_H
#define YACAS_MATHUSERFUNC_H

#include "yacasbase.h"
#include "lispuserfunc.h"
#include "patternclass.h"
#include "noncopyable.h"

#include <vector>

/// A mathematical function defined by several rules.
/// This is the basic class which implements functions in Yacas.
/// Evaluation is done by consulting a set of rewriting rules. The
/// body of the first rule that matches, is evaluated and this gives
/// the result of evaluating the function.

class BranchingUserFunction : public LispArityUserFunction
{
public:
  /// Structure containing name of parameter and whether it is put on hold.
  class BranchParameter : public YacasBase
  {
  public:
    BranchParameter(const LispString* aParameter = nullptr, LispInt aHold=false)
        : iParameter(aParameter), iHold(aHold) {}
    const LispString* iParameter;
    LispInt iHold;
  };

  /// Abstract base class for rules.
  class BranchRuleBase : public YacasBase
  {
  public:
    virtual ~BranchRuleBase() = default;
    virtual bool Matches(LispEnvironment& aEnvironment, LispPtr* aArguments) = 0;
    virtual LispInt Precedence() const = 0;
    virtual LispPtr& Body() = 0;
  };

  /// A rule with a predicate.
  /// This rule matches if the predicate evaluates to #true.
  class BranchRule : public BranchRuleBase
  {
  public:
    BranchRule(LispInt aPrecedence,LispPtr& aPredicate,LispPtr& aBody) : iPrecedence(aPrecedence),iBody(aBody),iPredicate(aPredicate)
    {
    }

    /// Return true if the rule matches.
    /// #iPredicate is evaluated in \a Environment. If the result
    /// IsTrue(), this function returns true.
    bool Matches(LispEnvironment& aEnvironment, LispPtr* aArguments);

    /// Access #iPrecedence.
    LispInt Precedence() const;

    /// Access #iBody.
    LispPtr& Body();
  protected:
    BranchRule() : iPrecedence(0),iBody(),iPredicate() {};
  protected:
    LispInt iPrecedence;
    LispPtr iBody;
    LispPtr iPredicate;
  };

  /// A rule that always matches.
  class BranchRuleTruePredicate : public BranchRule
  {
  public:
    BranchRuleTruePredicate(LispInt aPrecedence,LispPtr& aBody)
    {
      iPrecedence = aPrecedence;
      iBody = (aBody);
    }
    /// Return #true, always.
    bool Matches(LispEnvironment& aEnvironment, LispPtr* aArguments);
  };

  /// A rule which matches if the corresponding PatternClass matches.
  class BranchPattern : public BranchRuleBase, NonCopyable
  {
  public:
    /// Constructor.
    /// \param aPrecedence precedence of the rule
    /// \param aPredicate generic object of type \c Pattern
    /// \param aBody body of the rule
    BranchPattern(LispInt aPrecedence,LispPtr& aPredicate,LispPtr& aBody) : iPrecedence(aPrecedence),iBody(aBody),iPredicate(aPredicate),iPatternClass(nullptr)
    {
      GenericClass *gen = aPredicate->Generic();
      PatternClass* pat = dynamic_cast<PatternClass*>(gen);
      if (!pat)
        throw LispErrInvalidArg();
      iPatternClass = pat;
    }

    /// Return true if the corresponding pattern matches.
    bool Matches(LispEnvironment& aEnvironment, LispPtr* aArguments);

    /// Access #iPrecedence
    LispInt Precedence() const;

    /// Access #iBody
    LispPtr& Body();

  protected:
    /// The precedence of this rule.
    LispInt iPrecedence;

    /// The body of this rule.
    LispPtr iBody;

    /// Generic object of type \c Pattern containing #iPatternClass
    LispPtr iPredicate;

    /// The pattern that decides whether this rule matches.
    PatternClass *iPatternClass;
  };

  /// Constructor.
  /// \param aParameters linked list constaining the names of the arguments
  ///
  /// #iParamList and #iParameters are set from \a aParameters.
  BranchingUserFunction(LispPtr& aParameters);

  /// Destructor.
  ~BranchingUserFunction();

  /// Evaluate the function on given arguments.
  /// \param aResult (on output) the result of the evaluation
  /// \param aEnvironment the underlying Lisp environment
  /// \param aArguments the arguments to the function
  ///
  /// First, all arguments are evaluated by the evaluator associated
  /// to \a aEnvironment, unless the \c iHold flag of the
  /// corresponding parameter is true. Then a new LispLocalFrame is
  /// constructed, in which the actual arguments are assigned to the
  /// names of the formal arguments, as stored in \c iParameter. Then
  /// all rules in #iRules are tried one by one. The body of the
  /// first rule that matches is evaluated, and the result is put in
  /// \a aResult. If no rule matches, \a aResult will recieve a new
  /// expression with evaluated arguments.
  void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment, LispPtr& aArguments);

  /// Put an argument on hold.
  /// \param aVariable name of argument to put un hold
  ///
  /// The \c iHold flag of the corresponding argument is set. This
  /// implies that this argument is not evaluated by Evaluate().
  virtual void HoldArgument(const LispString* aVariable);

  /// Return true if the arity of the function equals \a aArity.
  virtual LispInt IsArity(LispInt aArity) const;

  /// Return the arity (number of arguments) of the function.
  LispInt Arity() const;

  /// Add a BranchRule to the list of rules.
  /// \sa InsertRule()
  virtual void DeclareRule(LispInt aPrecedence, LispPtr& aPredicate, LispPtr& aBody);

  /// Add a BranchRuleTruePredicate to the list of rules.
  /// \sa InsertRule()
  virtual void DeclareRule(LispInt aPrecedence, LispPtr& aBody);

  /// Add a BranchPattern to the list of rules.
  /// \sa InsertRule()
  void DeclarePattern(LispInt aPrecedence, LispPtr& aPredicate, LispPtr& aBody);

  /// Insert any BranchRuleBase object in the list of rules.
  /// This function does the real work for DeclareRule() and
  /// DeclarePattern(): it inserts the rule in #iRules, while
  /// keeping it sorted. The algorithm is \f$O(\log n)\f$, where
  /// \f$n\f$ denotes the number of rules.
  void InsertRule(LispInt aPrecedence,BranchRuleBase* newRule);

  /// Return the argument list, stored in #iParamList
  virtual const LispPtr& ArgList() const;

protected:
  /// List of arguments, with corresponding \c iHold property.
  std::vector<BranchParameter> iParameters;

  /// List of rules, sorted on precedence.
  std::vector<BranchRuleBase*> iRules;

  /// List of arguments
  LispPtr iParamList;
};

class ListedBranchingUserFunction : public BranchingUserFunction
{
public:
  ListedBranchingUserFunction(LispPtr& aParameters);
  LispInt IsArity(LispInt aArity) const;
  void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment, LispPtr& aArguments);
};


class MacroUserFunction : public BranchingUserFunction
{
public:
  MacroUserFunction(LispPtr& aParameters);
  void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment, LispPtr& aArguments);
};


class ListedMacroUserFunction : public MacroUserFunction
{
public:
  ListedMacroUserFunction(LispPtr& aParameters);
  LispInt IsArity(LispInt aArity) const;
  void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment, LispPtr& aArguments);
};




#endif

