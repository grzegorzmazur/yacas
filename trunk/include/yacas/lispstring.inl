
// LispString inline functions.

inline LispString& LispString::operator=(const LispString& aString)
{
  SetString(aString.c_str());
  return *this;
}

inline LispString& LispString::operator=(const LispChar* aString)
{
  SetString(aString);
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
  const LispChar s[1] = { 0 };
  SetString(s);
}

inline const LispChar * LispString::c_str() const
{
    return elements();
}
