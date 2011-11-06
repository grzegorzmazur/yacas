
#include <stdio.h>
#include <string.h>

class LispLocalFile : public LispBase
{
public:
  LispLocalFile(LispEnvironment& aEnvironment,
                LispChar * aFileName, LispBoolean aRead,
                InputDirectories& aInputDirectories);
  virtual ~LispLocalFile();
  virtual void Delete();
private:
  LispLocalFile(const LispLocalFile& aOther) : iFile(NULL),iEnvironment(aOther.iEnvironment),iOpened(LispFalse)
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
  FILE* iFile;
  LispEnvironment& iEnvironment;
  LispInt iOpened;
};


class StdFileInput : public LispInput
{
public:
    virtual LispChar Next();
    virtual LispChar Peek();
    virtual LispBoolean EndOfStream();
    void Rewind();
    virtual LispChar * StartPtr();
    virtual LispInt Position();
    virtual void SetPosition(LispInt aPosition);

protected:
    StdFileInput(FILE* aFile,InputStatus& aStatus);
    StdFileInput(LispLocalFile& aFile,InputStatus& aStatus);
protected:
    FILE* iFile;
};


/** CachedStdFileInput : same as StdFileInput, but with caching
 * for speed */
class CachedStdFileInput : public StdFileInput
{
public:
  CachedStdFileInput(LispLocalFile& aFile,InputStatus& aStatus);
  ~CachedStdFileInput() ;
  virtual LispChar Next();
  virtual LispChar Peek();
  virtual LispBoolean EndOfStream();
  void Rewind();
  virtual LispChar * StartPtr();
  virtual LispInt Position();
  virtual void SetPosition(LispInt aPosition);
private:
  inline CachedStdFileInput(const CachedStdFileInput& aOther) : StdFileInput(aOther),iBuffer(NULL),iCurrentPos(0),iNrBytes(0)
  {
    // copy constructor not written yet, hence the assert
    LISPASSERT(0);
  }
  inline CachedStdFileInput& operator=(const CachedStdFileInput& aOther)
  {
    // copy constructor not written yet, hence the assert
    LISPASSERT(0);
    return *this;
  }

private:
  LispChar * iBuffer;
  LispInt iCurrentPos;
  LispInt iNrBytes;
};

#define FILEINPUT CachedStdFileInput


class StdFileOutput : public LispOutput
{
public:
    StdFileOutput(LispLocalFile& aFile);
    StdFileOutput(FILE* aFile);
    virtual void PutChar(LispChar aChar);
public:
    FILE* iFile;
};

class StdUserOutput : public StdFileOutput
{
public:
    StdUserOutput() : StdFileOutput(stdout) {};
};

class StdUserInput : public StdFileInput
{
public:
    StdUserInput(InputStatus& aStatus) : StdFileInput(stdin,aStatus) {};
};

class CachedStdUserInput : public StdUserInput
{
public:
    CachedStdUserInput(InputStatus& aStatus);
public:
    virtual LispChar Next();
    virtual LispChar Peek();
    virtual LispBoolean EndOfStream();
    void Rewind();
    virtual LispChar * StartPtr();
    virtual LispInt Position();

private:
    LispString iBuffer;
    LispInt iCurrentPos;
};



void InternalFindFile(LispChar * aFileName, InputDirectories& aInputDirectories,
                     LispChar * aFoundFile);




