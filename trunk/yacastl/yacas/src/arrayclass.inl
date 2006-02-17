
inline LispInt ArrayClass::Size()
{
    return iArray.Size();
}

inline LispObject* ArrayClass::GetElement(LispInt aItem)
{
    LISPASSERT(aItem>0 && aItem<=iArray.Size());
	#if HAS_NEW_LispPtrArray == 0
		return iArray.GetElement(aItem-1);
	#else
		//LISPASSERT(aItem-1>=0 && aItem-1<iArray.Size());
		return iArray[aItem-1];
	#endif
}

inline void ArrayClass::SetElement(LispInt aItem,LispObject* aObject)
{
    LISPASSERT(aItem>0 && aItem<=iArray.Size());
	#if HAS_NEW_LispPtrArray == 0
	    iArray.SetElement(aItem-1,aObject);
	#else
		//LISPASSERT(aItem-1>=0 && aItem-1<iArray.Size());
		iArray[aItem-1] = (aObject);
	#endif
}

