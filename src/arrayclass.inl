
inline LispInt ArrayClass::Size()
{
    return iArray.Size();
}

inline LispObject* ArrayClass::GetElement(LispInt aItem)
{
    LISPASSERT(aItem>0 && aItem<=iSize);
    return iArray.GetElement(aItem-1).Get();
}

inline void ArrayClass::SetElement(LispInt aItem,LispObject* aObject)
{
    LISPASSERT(aItem>0 && aItem<=iSize);
    iArray.SetElement(aItem-1,aObject);
}

