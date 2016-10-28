


inline LispPtr& Argument(LispPtr& cur, int n)
{
    assert(n>=0);

    LispPtr* loop = &cur;
    while(n--) loop = &(*loop)->Nixed();
    return *loop;
}

// Boolean operations
inline void InternalTrue(const LispEnvironment& aEnvironment, LispPtr& aResult)
{
    aResult = (aEnvironment.iTrue->Copy());
}

inline void InternalFalse(const LispEnvironment& aEnvironment, LispPtr& aResult)
{
    aResult = (aEnvironment.iFalse->Copy());
}

inline void InternalBoolean(LispEnvironment& aEnvironment, LispPtr& aResult,
                            bool aValue)
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


inline bool IsTrue(LispEnvironment& aEnvironment, const LispPtr& aExpression)
{
    assert(aExpression);
    return aExpression->String() == aEnvironment.iTrue->String();
}
inline bool IsFalse(LispEnvironment& aEnvironment, const LispPtr& aExpression)
{
    assert(aExpression);
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
        if (!IsFalse(aEnvironment, aExpression))
            throw LispErrInvalidArg();

        InternalTrue(aEnvironment,aResult);
    }
}
