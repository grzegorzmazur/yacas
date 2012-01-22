
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

inline LispString::LispString(const LispString &aString) : iReferenceCount()
{
  SetString(aString.c_str());
}

inline LispString::LispString(const LispChar * aString) : iReferenceCount()
{
  SetString(aString);
}

inline LispString::LispString() : iReferenceCount()
{
  LispChar s[1] = { 0 };
  SetString(s);
}

inline LispChar * LispString::c_str() const
{
    return elements();
}
