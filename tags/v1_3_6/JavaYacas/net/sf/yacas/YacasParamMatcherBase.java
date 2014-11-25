package net.sf.yacas;


/// Abstract class for matching one argument to a pattern.
abstract class YacasParamMatcherBase
{
    /// Check whether some expression matches to the pattern.
    /// \param aEnvironment the underlying Lisp environment.
    /// \param aExpression the expression to test.
    /// \param arguments (input/output) actual values of the pattern
    /// variables for \a aExpression.
    public abstract boolean ArgumentMatches(LispEnvironment  aEnvironment,
                                        LispPtr  aExpression,
                                        LispPtr[]  arguments) throws Exception;
}
