
// LispString inline functions.

inline LispString& LispString::operator=(LispCharPtr aString)
{
    SetString(aString,iArrayOwnedExternally);
    return *this;
}

inline LispString::LispString(LispString &aString,
                              LispBoolean aStringOwnedExternally)
{
    SetString(aString.String(), aStringOwnedExternally);
}

inline LispString::LispString(LispCharPtr aString,
                              LispBoolean aStringOwnedExternally)
{
    SetString(aString, aStringOwnedExternally);
}

inline LispString::LispString(LispBoolean aStringOwnedExternally)
{
    SetString("", aStringOwnedExternally);
}


inline LispCharPtr LispString::String() const
{
    return (LispCharPtr)iArray;
}

