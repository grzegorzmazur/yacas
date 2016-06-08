package net.sf.yacas;


/// User function with a specific arity.
/// This is still an abstract class, but the arity (number of
/// arguments) of the function is now fixed.

abstract class LispArityUserFunction extends LispUserFunction
{
    public abstract int Arity();
    public abstract boolean IsArity(int aArity);
};
