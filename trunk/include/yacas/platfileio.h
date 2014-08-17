#ifndef YACAS_PLATFILEIO_H
#define YACAS_PLATFILEIO_H

#include <fstream>
#include <iostream>

class LispLocalFile: public LispBase {
public:
    LispLocalFile(
        LispEnvironment& environment,
        const LispChar* fname,
        bool read,
        const std::vector<std::string>& dirs);
    virtual ~LispLocalFile();

    virtual void Delete();

private:
    // not implemented
    LispLocalFile(const LispLocalFile& aOther);
    LispLocalFile& operator=(const LispLocalFile&);

public:
    std::fstream stream;
    LispEnvironment& environment;
};


class StdFileInput : public LispInput
{
public:
  virtual LispChar Next();
  virtual LispChar Peek();
  virtual bool EndOfStream();
  void Rewind();
  virtual const LispChar* StartPtr();
  virtual LispInt Position();
  virtual void SetPosition(LispInt aPosition);

protected:
    StdFileInput(std::istream&, InputStatus& aStatus);
    StdFileInput(LispLocalFile& aFile,InputStatus& aStatus);

    std::istream& stream;
};


/** CachedStdFileInput : same as StdFileInput, but with caching
 * for speed */
class CachedStdFileInput: public StdFileInput {
public:
    CachedStdFileInput(LispLocalFile& aFile,InputStatus& aStatus);
    ~CachedStdFileInput() ;
    virtual LispChar Next();
    virtual LispChar Peek();
    virtual bool EndOfStream() const;
    void Rewind();
    virtual const LispChar* StartPtr();
    virtual LispInt Position();
    virtual void SetPosition(LispInt aPosition);

private:
    // not implemented
    CachedStdFileInput(const CachedStdFileInput&);
    CachedStdFileInput& operator=(const CachedStdFileInput&);

    LispChar* iBuffer;
    LispInt iCurrentPos;
    LispInt iNrBytes;
};

class StdFileOutput: public LispOutput {
public:
    StdFileOutput(LispLocalFile& aFile);
    StdFileOutput(std::ostream&);

    virtual void PutChar(LispChar aChar);

    std::ostream& stream;
};

class StdUserOutput: public StdFileOutput {
public:
    StdUserOutput():
        StdFileOutput(std::cout)
    {
    }
};

class StdUserInput: public StdFileInput {
public:
    StdUserInput(InputStatus& aStatus):
        StdFileInput(std::cin, aStatus)
    {
    }
};

class CachedStdUserInput: public StdUserInput {
public:
    CachedStdUserInput(InputStatus& aStatus);

    virtual LispChar Next();
    virtual LispChar Peek();
    virtual bool EndOfStream() const;
    void Rewind();
    virtual const LispChar* StartPtr();
    virtual LispInt Position();

private:
    LispString iBuffer;
    std::size_t iCurrentPos;
};


std::string InternalFindFile(const LispChar* fname, const std::vector<std::string>& dirs);

#endif
