


inline LispPtr& Argument(LispPtr& cur, LispInt n)
{
    LISPASSERT(n>=0);

    LispPtr* loop = &cur;
    while(n--) loop = &(*loop)->Nixed();
    return *loop;
}

// Boolean operations
inline void InternalTrue(LispEnvironment& aEnvironment, LispPtr& aResult)
{
    aResult = (aEnvironment.iTrue->Copy());
}

inline void InternalFalse(LispEnvironment& aEnvironment, LispPtr& aResult)
{
    aResult = (aEnvironment.iFalse->Copy());
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
    LISPASSERT(aExpression);
    return aExpression->String() == aEnvironment.iTrue->String();
}
inline LispBoolean IsFalse(LispEnvironment& aEnvironment, LispPtr& aExpression)
{
    LISPASSERT(aExpression);
    return aExpression->String() == aEnvironment.iFalse->String();
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


