
#include <stdio.h>
#include <string.h>

class LispLocalFile : public LispBase
{
public:
    LispLocalFile(LispEnvironment& aEnvironment,
                  LispCharPtr aFileName, LispBoolean aRead,
                  InputDirectories& aInputDirectories);
    virtual ~LispLocalFile();
    virtual void Delete();

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
    virtual LispCharPtr StartPtr();
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
    virtual LispCharPtr StartPtr();
    virtual LispInt Position();
    
private:
    LispCharPtr iBuffer;
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
    virtual LispCharPtr StartPtr();
    virtual LispInt Position();

private:
    LispString iBuffer;
    LispInt iCurrentPos;
};



void InternalFindFile(LispCharPtr aFileName, InputDirectories& aInputDirectories,
                     LispCharPtr aFoundFile);




