#ifndef YACAS_PLATFILEIO_H
#define YACAS_PLATFILEIO_H

#include "lispenvironment.h"
#include "noncopyable.h"

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

class StdFileInput: public LispInput {
public:
    StdFileInput(std::istream&, InputStatus& aStatus);
    StdFileInput(LispLocalFile& aFile,InputStatus& aStatus);

    char32_t Next() override;
    char32_t Peek() override;
    bool EndOfStream() const override;
    virtual void Rewind();
    std::size_t Position() const override;
    void SetPosition(std::size_t) override;
    
private:
    void _get() const;

    std::istream& _stream;
    std::size_t _position;
    mutable bool _cp_ready;
    mutable char32_t _cp;
};

class StdUserInput final: public StdFileInput {
public:
    StdUserInput(InputStatus& aStatus):
        StdFileInput(std::cin, aStatus)
    {
    }
};


std::string InternalFindFile(const std::string& fname, const std::vector<std::string>& dirs);

#endif
