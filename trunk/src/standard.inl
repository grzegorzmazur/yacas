


inline LispPtr& Argument(LispPtr& cur, LispInt n)
{
    LISPASSERT(n>=0);

    LispPtr* loop = &cur;
    while(n--) loop = &loop->Get()->Next();
    return *loop;
}

// Boolean operations
inline void InternalTrue(LispEnvironment& aEnvironment, LispPtr& aResult)
{               
    aResult.Set(aEnvironment.iTrueAtom.Get()->Copy(LispFalse));
}

inline void InternalFalse(LispEnvironment& aEnvironment, LispPtr& aResult)
{
    aResult.Set(aEnvironment.iFalseAtom.Get()->Copy(LispFalse));
}

inline void InternalBoolean(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispBoolean aValue)
{
    if (aValue)
    {
        InternalTrue(aEnvironment, aResult);
    }
    else
    {
        InternalFalse(aEnvironment, aResult);
    }
}


inline LispBoolean IsTrue(LispEnvironment& aEnvironment, LispPtr& aExpression)
{
    LISPASSERT(aExpression.Get() != NULL);
    return aExpression.Get()->String() == aEnvironment.iTrue;
}
inline LispBoolean IsFalse(LispEnvironment& aEnvironment, LispPtr& aExpression)
{
    LISPASSERT(aExpression.Get() != NULL);
    return aExpression.Get()->String() == aEnvironment.iFalse;
}


inline void InternalNot(LispPtr& aResult, LispEnvironment& aEnvironment, LispPtr& aExpression)
{
    if (IsTrue(aEnvironment, aExpression))
    {
        InternalFalse(aEnvironment,aResult);
    }
    else
    {
        Check(IsFalse(aEnvironment, aExpression),KLispErrInvalidArg);
        InternalTrue(aEnvironment,aResult);
    }
}


