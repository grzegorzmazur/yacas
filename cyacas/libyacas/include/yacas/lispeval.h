/** \file lispeval.h
 *  Evaluation of expressions.
 *
 */

#ifndef YACAS_LISPEVAL_H
#define YACAS_LISPEVAL_H

#include "lispobject.h"
#include "lispenvironment.h"

class UserStackInformation {
public:
    UserStackInformation()
      : iOperator(),iExpression(),iRulePrecedence(-1),iSide(0)
    {
    }
    LispPtr iOperator;
    LispPtr iExpression;
    int iRulePrecedence;
    int iSide; // 0=pattern, 1=body
};

/// Abstract evaluator for Lisp expressions.
/// Eval() is a pure virtual function, to be provided by the derived class.
/// The other functions are stubs.

class LispEvaluatorBase {
public:
  LispEvaluatorBase() : iBasicInfo() {}
  virtual ~LispEvaluatorBase() = default;
  virtual void Eval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression)=0;
  virtual void ResetStack();
  virtual UserStackInformation& StackInformation();
  virtual void ShowStack(LispEnvironment& aEnvironment, std::ostream& aOutput);
private:
  UserStackInformation iBasicInfo;
};

/// The basic evaluator for Lisp expressions.

class BasicEvaluator : public LispEvaluatorBase
{
public:
  /// Evaluate a Lisp expression
  /// \param aEnvironment the Lisp environment, in which the
  /// evaluation should take place.
  /// \param aResult the result of the evaluation.
  /// \param aExpression the expression to evaluate.
  ///
  /// First, the evaluation depth is checked. An error is raised if
  /// the maximum evaluation depth is exceeded.
  ///
  /// The next step is the actual evaluation. \a aExpression is a
  /// LispObject, so we can distinguish three cases.
  ///   - If \a aExpression is a string starting with \c " , it is
  ///     simply copied in \a aResult. If it starts with another
  ///     character (this includes the case where it represents a
  ///     number), the environment is checked to see whether a
  ///     variable with this name exists. If it does, its value is
  ///     copied in \a aResult, otherwise \a aExpression is copied.
  ///   - If \a aExpression is a list, the head of the list is
  ///     examined. If the head is not a string. InternalApplyPure()
  ///     is called. If the head is a string, it is checked against
  ///     the core commands; if there is a check, the corresponding
  ///     evaluator is called. Then it is checked agaist the list of
  ///     user function with GetUserFunction() . Again, the
  ///     corresponding evaluator is called if there is a check. If
  ///     all fails, ReturnUnEvaluated() is called.
  ///   - Otherwise (ie. if \a aExpression is a generic object), it is
  ///     copied in \a aResult.
  ///
  /// \note The result of this operation must be a unique (copied)
  /// element! Eg. its Next might be set...
  ///
  /// The LispPtr it can be stored in to is passed in as argument, so it
  /// does not need to be constructed by the calling environment.
  void Eval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression) override;
};

class TracedEvaluator : public BasicEvaluator
{
public:
  void Eval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression) override;
protected:
  std::ostringstream errorOutput;
};


class TracedStackEvaluator final: public BasicEvaluator
{
public:
  TracedStackEvaluator() : objs() {}
  ~TracedStackEvaluator() override;
  void Eval(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression) override;
  void ResetStack() override;
  UserStackInformation& StackInformation() override;
  void ShowStack(LispEnvironment& aEnvironment, std::ostream& aOutput) override;
private:
  void PushFrame();
  void PopFrame();
private:
  std::vector<UserStackInformation*> objs;
};



/* GetUserFunction : get user function, possibly loading the required
   files to read in the function definition */
LispUserFunction* GetUserFunction(LispEnvironment& aEnvironment,
                                  LispPtr* subList);


/* Tracing functions */
void TraceShowEnter(LispEnvironment& aEnvironment,
                    LispPtr& aExpression);
void TraceShowLeave(LispEnvironment& aEnvironment, LispPtr& aResult,
                    LispPtr& aExpression);
void TraceShowArg(LispEnvironment& aEnvironment,LispPtr& aParam,
                  LispPtr& aValue);


void ShowExpression(LispString& outString, LispEnvironment& aEnvironment,
                    LispPtr& aExpression);


class YacasDebuggerBase {
public:
  virtual ~YacasDebuggerBase() = default;
  virtual void Start() = 0;
  virtual void Finish() = 0;
  virtual void Enter(LispEnvironment& aEnvironment,
                     LispPtr& aExpression) = 0;
  virtual void Leave(LispEnvironment& aEnvironment, LispPtr& aResult,
                     LispPtr& aExpression) = 0;
  virtual void Error(LispEnvironment& aEnvironment) = 0;
  virtual bool Stopped() = 0;
};

class DefaultDebugger final: public YacasDebuggerBase
{
public:
  DefaultDebugger(LispPtr& aEnter, LispPtr& aLeave, LispPtr& aError)
    : iEnter(aEnter), iLeave(aLeave), iError(aError), iTopExpr(),iTopResult(),iStopped(false),defaultEval() {};
  void Start() override;
  void Finish() override;
  void Enter(LispEnvironment& aEnvironment, LispPtr& aExpression) override;
  void Leave(LispEnvironment& aEnvironment, LispPtr& aResult, LispPtr& aExpression) override;
  void Error(LispEnvironment& aEnvironment) override;
  bool Stopped() override;
  LispPtr iEnter;
  LispPtr iLeave;
  LispPtr iError;
  LispPtr iTopExpr;
  LispPtr iTopResult;
  bool iStopped;
protected:
  BasicEvaluator defaultEval;
};


#endif



