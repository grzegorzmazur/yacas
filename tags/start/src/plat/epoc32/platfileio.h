
#include <F32FILE.H>
#include "stringio.h"


class LispLocalFile : public LispBase
{
public:
    LispLocalFile(LispEnvironment& aEnvironment,
                  LispCharPtr aFileName, LispBoolean aRead,
                  InputDirectories& aInputDirectories);
    virtual ~LispLocalFile();
    virtual void Delete();

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
    virtual LispCharPtr StartPtr();
    virtual LispInt Position();
protected:
    RFile &iFile;
    LispCharPtr iBuffer;
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
    virtual LispCharPtr StartPtr();
    virtual LispInt Position();
};

void InternalFindFile(LispCharPtr aFileName, InputDirectories& aInputDirectories,
                     LispCharPtr aFoundFile);





