
#ifndef __standard_h__
#define __standard_h__

#include "lispobject.h"
#include "lispenvironment.h"
#include "lisphash.h"
#include "lispatom.h"
#include "numbers.h"


// Prototypes
class LispHashTable;

LispBoolean InternalIsList(LispPtr& aPtr);
LispBoolean InternalIsString(LispStringPtr aOriginal);
void InternalUnstringify(LispString& aResult, LispStringPtr aOriginal);
void InternalStringify(LispString& aResult, LispStringPtr aOriginal);
void InternalIntToAscii(LispCharPtr aTrg,LispInt aInt);
LispInt InternalAsciiToInt(LispCharPtr aString);
LispBoolean IsNumber(LispCharPtr ptr,LispBoolean aAllowFloat);

void InternalNth(LispPtr& aResult, LispPtr& aArg, LispInt n);
void InternalTail(LispPtr& aResult, LispPtr& aArg);
void InternalAssociate(LispPtr& aResult, LispPtr& aKey,
                      LispPtr& aAssociationList);

void InternalReverseList(LispPtr& aResult, LispPtr& aOriginal);
void InternalFlatCopy(LispPtr& aResult, LispPtr& aOriginal);
LispInt InternalListLength(LispPtr& aOriginal);

LispBoolean InternalEquals(LispEnvironment& aEnvironment,
                           LispPtr& aExpression1,
                           LispPtr& aExpression2);


inline LispPtr& Argument(LispPtr& cur, LispInt n);
inline void InternalTrue(LispEnvironment& aEnvironment, LispPtr& aResult);
inline void InternalFalse(LispEnvironment& aEnvironment, LispPtr& aResult);
inline void InternalBoolean(LispEnvironment& aEnvironment, LispPtr& aResult,
                            LispBoolean aValue);
inline LispBoolean IsTrue(LispEnvironment& aEnvironment, LispPtr& aExpression);
inline LispBoolean IsFalse(LispEnvironment& aEnvironment, LispPtr& aExpression);
inline void InternalNot(LispPtr& aResult, LispEnvironment& aEnvironment, LispPtr& aExpression);

void DoInternalLoad(LispEnvironment& aEnvironment,LispInput* aInput);
void InternalLoad(LispEnvironment& aEnvironment,LispStringPtr aFileName);
void InternalUse(LispEnvironment& aEnvironment,LispStringPtr aFileName);
void InternalApplyString(LispEnvironment& aEnvironment, LispPtr& aResult,
                         LispStringPtr aOperator,LispPtr& aArgs);

void InternalEvalString(LispEnvironment& aEnvironment, LispPtr& aResult,
                        LispCharPtr aString);


#include "standard.inl"


#endif
