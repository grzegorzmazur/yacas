
// LispString inline functions.

inline LispString& LispString::operator=(const LispString& aString)
{
  assign(aString.c_str());
  return *this;
}

inline LispString& LispString::operator=(const LispChar* aString)
{
  assign(aString);
  return *this;
}

inline LispString::LispString(const LispString& s):
    std::string(s.c_str()),
    iReferenceCount(0)
{
}

inline LispString::LispString(const LispChar* s):
    std::string(s),
    iReferenceCount(0)
{
}

inline LispString::LispString():
    iReferenceCount(0)
{
}
