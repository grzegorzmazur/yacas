
inline LispInt ArrayClass::Size()
{
    return iArray.Size();
}

inline LispObject* ArrayClass::GetElement(LispInt aItem)
{
  LISPASSERT(aItem>0 && aItem<=iArray.Size());
  //LISPASSERT(aItem-1>=0 && aItem-1<iArray.Size());
  return iArray[aItem-1];
}

inline void ArrayClass::SetElement(LispInt aItem,LispObject* aObject)
{
  LISPASSERT(aItem>0 && aItem<=iArray.Size());
  //LISPASSERT(aItem-1>=0 && aItem-1<iArray.Size());
  iArray[aItem-1] = (aObject);
}

