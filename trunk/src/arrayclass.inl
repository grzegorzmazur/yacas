
inline LispInt ArrayClass::Size()
{
    return iSize;
}

inline LispObject* ArrayClass::GetElement(LispInt aItem)
{
    LISPASSERT(aItem>0 && aItem<=iSize);
    return iArray[aItem-1].Get();
}

inline void ArrayClass::SetElement(LispInt aItem,LispObject* aObject)
{
    LISPASSERT(aItem>0 && aItem<=iSize);
    iArray[aItem-1].Set(aObject);
}

