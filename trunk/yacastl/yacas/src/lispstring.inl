
// LispString inline functions.

inline void LispString::SetString(LispChar * aString, LispBoolean aStringOwnedExternally)
{
    if (aStringOwnedExternally)
    {
		LispInt length = PlatStrLen(aString);  // my own strlen
        SetExternalArray(aString, length+1);
    }
    else
    {
		SetString(aString);
    }
}

inline LispString& LispString::operator=(LispChar * aString)
{
    SetString(aString,ArrayOwnedExternally());
    return *this;
}

inline LispString::LispString(LispString &aString, LispBoolean aStringOwnedExternally)
{
    SetString(aString.c_str(), aStringOwnedExternally);
}

inline LispString::LispString(const LispChar * aString)
{
    SetString(aString);
}

inline LispString::LispString(LispChar * aString, LispBoolean aStringOwnedExternally)
{
    SetString(aString, aStringOwnedExternally);
}

inline LispString::LispString(LispBoolean aStringOwnedExternally)
{
    SetString("", aStringOwnedExternally);
}

inline LispChar * LispString::c_str() const
{
    return elements();
}
