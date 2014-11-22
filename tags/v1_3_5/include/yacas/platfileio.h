#ifndef YACAS_PLATFILEIO_H
#define YACAS_PLATFILEIO_H

#include <fstream>
#include <iostream>

class LispLocalFile: public LispBase {
public:
    LispLocalFile(
        LispEnvironment& environment,
        const std::string& fname,
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
  virtual std::size_t Position();
  virtual void SetPosition(std::size_t aPosition);

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

    LispChar Next();
    LispChar Peek();
    bool EndOfStream() const;
    void Rewind();
    const LispChar* StartPtr();
    std::size_t Position() const;
    void SetPosition(std::size_t aPosition);

private:
    // not implemented
    CachedStdFileInput(const CachedStdFileInput&);
    CachedStdFileInput& operator=(const CachedStdFileInput&);

    std::vector<LispChar> _buf;
    std::size_t iCurrentPos;
};

//class StdFileOutput: public LispOutput {
//public:
//    StdFileOutput(LispLocalFile& aFile);
//    StdFileOutput(std::ostream&);
//
//    virtual void PutChar(LispChar aChar);
//
//    std::ostream& stream;
//};
//
//class StdUserOutput: public StdFileOutput {
//public:
//    StdUserOutput():
//        StdFileOutput(std::cout)
//    {
//    }
//};

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
    virtual std::size_t Position() const;

private:
    LispString iBuffer;
    std::size_t iCurrentPos;
};


std::string InternalFindFile(const LispChar* fname, const std::vector<std::string>& dirs);

#endif
