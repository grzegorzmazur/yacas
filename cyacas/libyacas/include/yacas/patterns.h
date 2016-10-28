#ifndef YACAS_PATTERNS_H
#define YACAS_PATTERNS_H

/// \file
/// Pattern matching code.
///
/// General idea: have a class that can match function parameters
/// to a pattern, check for predicates on the arguments, and return
/// whether there was a match.
///
/// First the pattern is mapped onto the arguments. Then local variables
/// are set. Then the predicates are called. If they all return true,
/// Then the pattern matches, and the locals can stay (the body is expected
/// to use these variables).


#include "lisptype.h"
#include "lispenvironment.h"
#include "noncopyable.h"
#include "numbers.h"

#include <vector>

/// Abstract class for matching one argument to a pattern.
class YacasParamMatcherBase {
public:
    /// Destructor.
    virtual ~YacasParamMatcherBase() = default;

    /// Check whether some expression matches to the pattern.
    /// \param aEnvironment the underlying Lisp environment.
    /// \param aExpression the expression to test.
    /// \param arguments (input/output) actual values of the pattern
    /// variables for \a aExpression.
    virtual bool ArgumentMatches(LispEnvironment& aEnvironment,
                                 LispPtr& aExpression,
                                 LispPtr* arguments) const = 0;
};

/// Class for matching an expression to a given atom.
class MatchAtom : public YacasParamMatcherBase
{
public:
    explicit MatchAtom(const LispString* aString);
    bool ArgumentMatches(LispEnvironment& aEnvironment,
                         LispPtr& aExpression,
                         LispPtr* arguments) const override;
protected:
    const LispString* iString;
};

inline
MatchAtom::MatchAtom(const LispString* aString):
    iString(aString)
{
}

/// Class for matching an expression to a given number.
class MatchNumber : public YacasParamMatcherBase
{
public:
    explicit MatchNumber(BigNumber* aNumber);
    bool ArgumentMatches(LispEnvironment& aEnvironment,
                         LispPtr& aExpression,
                         LispPtr* arguments) const override;
protected:
    RefPtr<BigNumber> iNumber;
};

inline
MatchNumber::MatchNumber(BigNumber* aNumber):
    iNumber(aNumber)
{
}

/// Class for matching against a list of YacasParamMatcherBase objects.
class MatchSubList final: public YacasParamMatcherBase, NonCopyable {
public:
    explicit MatchSubList(const std::vector<const YacasParamMatcherBase*>&& aMatchers);
    ~MatchSubList() override;

    bool ArgumentMatches(
        LispEnvironment& aEnvironment,
        LispPtr& aExpression,
        LispPtr* arguments) const override;

protected:
    std::vector<const YacasParamMatcherBase*> iMatchers;
};

inline
MatchSubList::MatchSubList(const std::vector<const YacasParamMatcherBase*>&& aMatchers):
    iMatchers(aMatchers)
{
}

inline
MatchSubList::~MatchSubList()
{
    for (const YacasParamMatcherBase* m: iMatchers)
        delete m;
}

/// Class for matching against a pattern variable.
class MatchVariable final: public YacasParamMatcherBase
{
public:
    explicit MatchVariable(int aVarIndex);

    /// Matches an expression against the pattern variable.
    /// \param aEnvironment the underlying Lisp environment.
    /// \param aExpression the expression to test.
    /// \param arguments (input/output) actual values of the pattern
    /// variables for \a aExpression.
    ///
    /// If entry #iVarIndex in \a arguments is still empty, the
    /// pattern matches and \a aExpression is stored in this
    /// entry. Otherwise, the pattern only matches if the entry equals
    /// \a aExpression.
    bool ArgumentMatches(LispEnvironment& aEnvironment,
                         LispPtr& aExpression,
                         LispPtr* arguments) const override;
protected:
    /// Index of variable in YacasPatternPredicateBase::iVariables.
    int iVarIndex;
};

inline
MatchVariable::MatchVariable(int aVarIndex):
    iVarIndex(aVarIndex)
{
}

/// Class that matches function arguments to a pattern.
/// This class (specifically, the Matches() member function) can match
/// function parameters to a pattern, check for predicates on the
/// arguments, and return whether there was a match.
class YacasPatternPredicateBase: NonCopyable {
public:
    /// Constructor.
    /// \param aEnvironment the underlying Lisp environment
    /// \param aPattern Lisp expression containing the pattern
    /// \param aPostPredicate Lisp expression containing the
    /// postpredicate
    ///
    /// The function MakePatternMatcher() is called for every argument
    /// in \a aPattern, and the resulting pattern matchers are
    /// collected in #iParamMatchers. Additionally, \a aPostPredicate
    /// is copied, and the copy is added to #iPredicates.
    YacasPatternPredicateBase(LispEnvironment& aEnvironment,
                              LispPtr& aPattern,
                              LispPtr& aPostPredicate);

    /// Destructor.
    virtual ~YacasPatternPredicateBase();

    /// Try to match the pattern against \a aArguments.
    /// First, every argument in \a aArguments is matched against the
    /// corresponding YacasParamMatcherBase in #iParamMatches. If any
    /// match fails, Matches() returns false. Otherwise, a temporary
    /// LispLocalFrame is constructed, then SetPatternVariables() and
    /// CheckPredicates() are called, and then the LispLocalFrame is
    /// immediately deleted. If CheckPredicates() returns false, this
    /// function also returns false. Otherwise, SetPatternVariables()
    /// is called again, but now in the current LispLocalFrame, and
    /// this function returns true.
    bool Matches(LispEnvironment& aEnvironment, LispPtr& aArguments);

    /// Try to match the pattern against \a aArguments.
    /// This function does the same as Matches(LispEnvironment&,LispPtr&),
    /// but differs in the type of the arguments.
    bool Matches(LispEnvironment& aEnvironment, LispPtr* aArguments);

protected:
    /// Construct a pattern matcher out of a Lisp expression.
    /// The result of this function depends on the value of \a aPattern:
    /// - If \a aPattern is a number, the corresponding MatchNumber is
    ///   constructed and returned.
    /// - If \a aPattern is an atom, the corresponding MatchAtom is
    ///   constructed and returned.
    /// - If \a aPattern is a list of the form <tt>( _ var )<tt>,
    ///   where \c var is an atom, LookUp() is called on \c var. Then
    ///   the correspoding MatchVariable is constructed and returned.
    /// - If \a aPattern is a list of the form <tt>( _ var expr )<tt>,
    ///   where \c var is an atom, LookUp() is called on \c var. Then,
    ///   \a expr is appended to #iPredicates. Finally, the
    ///   correspoding MatchVariable is constructed and returned.
    /// - If \a aPattern is a list of another form, this function
    ///   calls itself on any of the entries in this list. The
    ///   resulting YacasParamMatcherBase objects are collected in a
    ///   MatchSubList, which is returned.
    /// - Otherwise, this function returns #nullptr.
    const YacasParamMatcherBase* MakeParamMatcher(LispEnvironment& aEnvironment, LispObject* aPattern);

    /// Look up a variable name in #iVariables
    /// \returns index in #iVariables array where \a aVariable
    /// appears.
    ///
    /// If \a aVariable is not in #iVariables, it is added.
    int LookUp(const LispString* aVariable);

protected:
    /// Set local variables corresponding to the pattern variables.
    /// This function goes through the #iVariables array. A local
    /// variable is made for every entry in the array, and the
    /// corresponding argument is assigned to it.
    void SetPatternVariables(LispEnvironment& aEnvironment, LispPtr* arguments);

    /// Check whether all predicates are true.
    /// This function goes through all predicates in #iPredicates, and
    /// evaluates them. It returns #false if at least one
    /// of these results IsFalse(). An error is raised if any result
    /// neither IsTrue() nor IsFalse().
    bool CheckPredicates(LispEnvironment& aEnvironment);

protected:
    /// List of parameter matches, one for every parameter.
    std::vector<const YacasParamMatcherBase*> iParamMatchers;

    /// List of variables appearing in the pattern.
    std::vector<const LispString*> iVariables;

    /// List of predicates which need to be true for a match.
    std::vector<LispPtr> iPredicates;
};


#endif
