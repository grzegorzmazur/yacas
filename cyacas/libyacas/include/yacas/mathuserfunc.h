#ifndef YACAS_MATHUSERFUNC_H
#define YACAS_MATHUSERFUNC_H

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
  class BranchParameter {
  public:
    BranchParameter(const LispString* aParameter = nullptr, int aHold=false)
        : iParameter(aParameter), iHold(aHold) {}
    const LispString* iParameter;
    int iHold;
  };

  /// Abstract base class for rules.
  class BranchRuleBase {
  public:
    virtual ~BranchRuleBase() = default;
    virtual bool Matches(LispEnvironment& aEnvironment, LispPtr* aArguments) = 0;
    virtual int Precedence() const = 0;
    virtual LispPtr& Body() = 0;
  };

  /// A rule with a predicate.
  /// This rule matches if the predicate evaluates to #true.
  class BranchRule : public BranchRuleBase
  {
  public:
    BranchRule(int aPrecedence,LispPtr& aPredicate,LispPtr& aBody) : iPrecedence(aPrecedence),iBody(aBody),iPredicate(aPredicate)
    {
    }

    /// Return true if the rule matches.
    /// #iPredicate is evaluated in \a Environment. If the result
    /// IsTrue(), this function returns true.
    bool Matches(LispEnvironment& aEnvironment, LispPtr* aArguments);

    /// Access #iPrecedence.
    int Precedence() const;

    /// Access #iBody.
    LispPtr& Body();
  protected:
    BranchRule() : iPrecedence(0),iBody(),iPredicate() {};
  protected:
    int iPrecedence;
    LispPtr iBody;
    LispPtr iPredicate;
  };

  /// A rule that always matches.
  class BranchRuleTruePredicate : public BranchRule
  {
  public:
    BranchRuleTruePredicate(int aPrecedence,LispPtr& aBody)
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
    BranchPattern(int aPrecedence,LispPtr& aPredicate,LispPtr& aBody) : iPrecedence(aPrecedence),iBody(aBody),iPredicate(aPredicate),iPatternClass(nullptr)
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
    int Precedence() const;

    /// Access #iBody
    LispPtr& Body();

  protected:
    /// The precedence of this rule.
    int iPrecedence;

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
  void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment, LispPtr& aArguments) const override;

  /// Put an argument on hold.
  /// \param aVariable name of argument to put un hold
  ///
  /// The \c iHold flag of the corresponding argument is set. This
  /// implies that this argument is not evaluated by Evaluate().
  void HoldArgument(const LispString* aVariable) override;

  /// Return true if the arity of the function equals \a aArity.
  bool IsArity(int aArity) const override;

  /// Return the arity (number of arguments) of the function.
  int Arity() const override;

  /// Add a BranchRule to the list of rules.
  /// \sa InsertRule()
  void DeclareRule(int aPrecedence, LispPtr& aPredicate, LispPtr& aBody) override;

  /// Add a BranchRuleTruePredicate to the list of rules.
  /// \sa InsertRule()
  void DeclareRule(int aPrecedence, LispPtr& aBody) override;

  /// Add a BranchPattern to the list of rules.
  /// \sa InsertRule()
  void DeclarePattern(int aPrecedence, LispPtr& aPredicate, LispPtr& aBody) override;

  /// Insert any BranchRuleBase object in the list of rules.
  /// This function does the real work for DeclareRule() and
  /// DeclarePattern(): it inserts the rule in #iRules, while
  /// keeping it sorted. The algorithm is \f$O(\log n)\f$, where
  /// \f$n\f$ denotes the number of rules.
  void InsertRule(int aPrecedence,BranchRuleBase* newRule);

  /// Return the argument list, stored in #iParamList
  const LispPtr& ArgList() const override;

protected:
  /// List of arguments, with corresponding \c iHold property.
  std::vector<BranchParameter> iParameters;

  /// List of rules, sorted on precedence.
  std::vector<BranchRuleBase*> iRules;

  /// List of arguments
  LispPtr iParamList;
};

class ListedBranchingUserFunction final: public BranchingUserFunction
{
public:
  ListedBranchingUserFunction(LispPtr& aParameters);
  bool IsArity(int aArity) const override;
  void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment, LispPtr& aArguments) const override;
};


class MacroUserFunction : public BranchingUserFunction
{
public:
  MacroUserFunction(LispPtr& aParameters);
  void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment, LispPtr& aArguments) const override;
};


class ListedMacroUserFunction final: public MacroUserFunction
{
public:
  ListedMacroUserFunction(LispPtr& aParameters);
  bool IsArity(int aArity) const override;
  void Evaluate(LispPtr& aResult,LispEnvironment& aEnvironment, LispPtr& aArguments) const override;
};




#endif

