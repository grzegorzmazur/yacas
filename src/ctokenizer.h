
#ifndef _customparser_h_
#define _customparser_h_

#include "yacasbase.h"
#include "tokenizer.h"



class CTokenizer : public LispTokenizer
{
    
public:
  CTokenizer() : LispTokenizer(),iPreProcessLine(0), iEnvironment(NULL),iFunction() {}
  inline void SetRemarkReceiver(LispEnvironment& aEnvironment);
  /// NextToken returns a string representing the next token,
  /// or an empty list.
  virtual LispString * NextToken(LispInput& aInput,
                                  LispHashTable& aHashTable);
  virtual ~CTokenizer(){}

private:
  CTokenizer(const CTokenizer& aOther) : LispTokenizer(),iPreProcessLine(0), iEnvironment(NULL),iFunction()
  {
    // copy constructor has not been written yet, hence the assert
    LISPASSERT(0);
  }
  CTokenizer& operator=(const CTokenizer& aOther)
  {
    // copy constructor has not been written yet, hence the assert
    LISPASSERT(0);
    return *this;
  }
private:
  void EmitRemark(LispString * remark);
private:
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

