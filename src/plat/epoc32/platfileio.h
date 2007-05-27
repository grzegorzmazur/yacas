
#include <F32FILE.H>
#include "stringio.h"


class LispLocalFile : public LispBase
{
public:
  LispLocalFile(LispEnvironment& aEnvironment,
                LispChar * aFileName, LispBoolean aRead,
                InputDirectories& aInputDirectories);
  virtual ~LispLocalFile();
  virtual void Delete();
private:
  LispLocalFile(const LispLocalFile& aOther) : iEnvironment(aOther.iEnvironment),iOpened(LispFalse)
  {
    // copy constructor not written yet, hence the assert
    LISPASSERT(0);
  }
  LispLocalFile& operator=(const LispLocalFile& aOther)
  {
    // copy constructor not written yet, hence the assert
    LISPASSERT(0);
    return *this;
  }
public:

  RFs fs;
  RFile iFile;
  LispEnvironment& iEnvironment;
  LispInt iOpened;
};


class StdFileInput : public LispInput
{
public:
    StdFileInput(LispLocalFile& aFile,InputStatus& aStatus);
    ~StdFileInput();
    virtual LispChar Next();
    virtual LispChar Peek();
    virtual LispBoolean EndOfStream();
    void Rewind();
    virtual LispChar * StartPtr();
    virtual LispInt Position();
    virtual void SetPosition(LispInt aPosition);
protected:
    RFile &iFile;
    LispChar * iBuffer;
    LispInt iCurrentPos;
    LispInt iNrBytes;
};



#define FILEINPUT StdFileInput


class StdFileOutput : public LispOutput
{
public:
    StdFileOutput(LispLocalFile& aFile);
    virtual void PutChar(LispChar aChar);
public:
    RFile &iFile;
};

class StdUserOutput : public StringOutput
{
public:
    StdUserOutput() : StringOutput(iString) {};
    LispString iString;
};


// Epoc input : for now none...
class CachedStdUserInput : public LispInput
{
public:
    CachedStdUserInput(InputStatus& aStatus);
    virtual LispChar Next();
    virtual LispChar Peek();
    virtual LispBoolean EndOfStream();
    virtual LispChar * StartPtr();
    virtual LispInt Position();
};

void InternalFindFile(LispChar * aFileName, InputDirectories& aInputDirectories,
                     LispChar * aFoundFile);





