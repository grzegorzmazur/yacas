
#ifndef _customparser_h_
#define _customparser_h_

#include "yacasbase.h"
#include "tokenizer.h"



class CTokenizer : public LispTokenizer
{
    
public:
    CTokenizer() : iPreProcessLine(0), iEnvironment(NULL) {}
    inline void SetRemarkReceiver(LispEnvironment& aEnvironment);
    /// NextToken returns a string representing the next token,
    /// or an empty list.
    virtual LispStringPtr NextToken(LispInput& aInput,
                                    LispHashTable& aHashTable);
    virtual ~CTokenizer(){}

private:
    void EmitRemark(LispStringPtr remark);
private:
    LispString iToken; //Can be used as a token container.
    LispInt iPreProcessLine;
private:
    LispEnvironment* iEnvironment;
    LispPtr iFunction;
};

inline void CTokenizer::SetRemarkReceiver(LispEnvironment& aEnvironment)
{
    iEnvironment = &aEnvironment;
}

#endif

