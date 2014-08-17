
inline LispInt ArrayClass::Size()
{
    return iArray.size();
}

inline LispObject* ArrayClass::GetElement(LispInt aItem)
{
  assert(aItem>0 && aItem<=iArray.size());
  return iArray[aItem-1];
}

inline void ArrayClass::SetElement(LispInt aItem,LispObject* aObject)
{
  assert(aItem>0 && aItem<=iArray.size());
  iArray[aItem-1] = (aObject);
}

