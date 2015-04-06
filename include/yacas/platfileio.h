#ifndef YACAS_PLATFILEIO_H
#define YACAS_PLATFILEIO_H

#include "noncopyable.h"
#include "yacasbase.h"

#include <fstream>
#include <iostream>

class LispLocalFile: NonCopyable {
public:
    LispLocalFile(
        LispEnvironment& environment,
        const std::string& fname,
        bool read,
        const std::vector<std::string>& dirs);
    virtual ~LispLocalFile();

public:
    std::fstream stream;
    LispEnvironment& environment;
};


class StdFileInput : public LispInput
{
public:
  virtual LispChar Next();
  virtual LispChar Peek();
  virtual bool EndOfStream() const;
  void Rewind();
  virtual std::size_t Position() const;
  virtual void SetPosition(std::size_t aPosition);

protected:
    StdFileInput(std::istream&, InputStatus& aStatus);
    StdFileInput(LispLocalFile& aFile,InputStatus& aStatus);

    std::istream& stream;
};


/** CachedStdFileInput : same as StdFileInput, but with caching
 * for speed */
class CachedStdFileInput: public StdFileInput, NonCopyable {
public:
    CachedStdFileInput(LispLocalFile& aFile,InputStatus& aStatus);

    LispChar Next();
    LispChar Peek();
    bool EndOfStream() const;
    void Rewind();
    std::size_t Position() const;
    void SetPosition(std::size_t aPosition);

private:
    std::vector<LispChar> _buf;
    std::size_t iCurrentPos;
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
    virtual std::size_t Position() const;

private:
    LispString iBuffer;
    std::size_t iCurrentPos;
};


std::string InternalFindFile(const LispChar* fname, const std::vector<std::string>& dirs);

#endif
