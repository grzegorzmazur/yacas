#include "yacas/yacasprivate.h"
#include "yacas/stdfileio.h"

#ifdef _WIN32
#define MAP_TO_WIN32_PATH_SEPARATOR
#endif // WIN32

static void MapPathSeparators(std::string& filename)
{
#ifdef MAP_TO_WIN32_PATH_SEPARATOR
    for (std::size_t i = 0; i < filename.size(); ++i)
        if (filename[i] == '/')
            filename[i] = '\\';
#endif
}


StdFileInput::StdFileInput(std::istream& stream, InputStatus& aStatus):
    LispInput(aStatus),
    stream(stream)
{
}

StdFileInput::StdFileInput(LispLocalFile& file, InputStatus& aStatus):
    LispInput(aStatus),
    stream(file.stream)
{
}



LispChar StdFileInput::Next()
{
    const LispChar c = stream.get();
    if (c == '\n')
        iStatus.NextLine();
    return c;
}

LispChar StdFileInput::Peek()
{
    return stream.peek();
}

void StdFileInput::Rewind()
{
    stream.seekg(0);
}

bool StdFileInput::EndOfStream()
{
    return stream.eof();
}

const LispChar* StdFileInput::StartPtr()
{
    assert(0);
    return 0;
}

std::size_t StdFileInput::Position()
{
    assert(0);
    return 0;
}

void StdFileInput::SetPosition(std::size_t aPosition)
{
  assert(0);
}



StdFileOutput::StdFileOutput(std::ostream& stream):
    stream(stream)
{
}

StdFileOutput::StdFileOutput(LispLocalFile& file):
    stream(file.stream)
{
}

void StdFileOutput::PutChar(LispChar c)
{
    stream.put(c);
}







CachedStdFileInput::~CachedStdFileInput()
{
    PlatFree(iBuffer);
}

CachedStdFileInput::CachedStdFileInput(LispLocalFile& file, InputStatus& status):
    StdFileInput(file, status),
    iBuffer(0),
    iCurrentPos(0)
{
    // Get size of file
    stream.seekg(0, std::ios_base::end);
    iNrBytes = stream.tellg();
    stream.seekg(0);

    // Read in the full buffer
    char * ptr = PlatAllocN<char>(iNrBytes+1);      // sizeof(char) == 1
    iBuffer = (LispChar *)ptr;                      // sizeof(LispChar) == not so sure

    if (!ptr)
        throw LispErrNotEnoughMemory();

    stream.read(ptr, iNrBytes);

    // if (n != 1)
    //     throw LispErrReadingFile();

    ptr[iNrBytes] = '\0';
}

LispChar CachedStdFileInput::Next()
{
    LispChar c;
    assert(iCurrentPos < iNrBytes);
    c = iBuffer[iCurrentPos++];

    if (c == '\n')
    {
        iStatus.NextLine();
    }
    return c;
}

LispChar CachedStdFileInput::Peek()
{
    assert(iCurrentPos < iNrBytes);
    return iBuffer[iCurrentPos];
}


void CachedStdFileInput::Rewind()
{
  iCurrentPos = 0;
}

bool CachedStdFileInput::EndOfStream() const
{
    return (iCurrentPos >= iNrBytes);
}

const LispChar* CachedStdFileInput::StartPtr()
{
    return iBuffer;
}
std::size_t CachedStdFileInput::Position() const
{
    return iCurrentPos;
}
void CachedStdFileInput::SetPosition(std::size_t aPosition)
{
  assert(aPosition<iNrBytes);
  iCurrentPos = aPosition;
}

std::string InternalFindFile(const LispChar* fname, const std::vector<std::string>& dirs)
{
    std::string path(fname);

    MapPathSeparators(path);

    FILE* file = fopen(path.c_str(), "rb");
    for (std::size_t i = 0; !file && i < dirs.size(); ++i) {
        path = dirs[i];
        path += fname;
        MapPathSeparators(path);
        file = fopen(path.c_str(), "rb");
    }

    if (file)
        fclose(file);

    if (!file)
        return "";

    return path;
}

LispLocalFile::LispLocalFile(
    LispEnvironment& environment,
    const LispChar* fname,
    bool read,
    const std::vector<std::string>& dirs):
    environment(environment)
{
    std::string othername;

    if (read) {
        othername = fname;
        MapPathSeparators(othername);

        stream.open(othername, std::ios_base::in | std::ios_base::binary);

        for (std::size_t i = 0; !stream.is_open() && i < dirs.size(); ++i) {
            othername = dirs[i];
            othername += fname;
            MapPathSeparators(othername);
            stream.open(othername, std::ios_base::in | std::ios_base::binary);
        }
    } else {
        othername = fname;
        MapPathSeparators(othername);
        stream.open(othername, std::ios_base::out);
    }

    SAFEPUSH(iEnvironment, *this);
}

//aRead is for opening in read mode (otherwise opened in write mode)
LispLocalFile::~LispLocalFile()
{
  SAFEPOP(iEnvironment);
  Delete();
}

void LispLocalFile::Delete()
{
    if (stream.is_open())
        stream.close();
}




CachedStdUserInput::CachedStdUserInput(InputStatus& aStatus) :
StdUserInput(aStatus),iBuffer(),iCurrentPos(0)
{
  Rewind();
}
LispChar CachedStdUserInput::Next()
{
  LispChar c = Peek();
  iCurrentPos++;
  printf("%c",c);
  return c;
}

LispChar CachedStdUserInput::Peek()
{
    if (iCurrentPos == iBuffer.size())
        iBuffer.push_back(stream.get());

    return iBuffer[iCurrentPos];
}

bool CachedStdUserInput::EndOfStream() const
{
  return false;
}

void CachedStdUserInput::Rewind()
{
  // Make sure there is a buffer to point to.
  iBuffer.resize(10);
  iBuffer.resize(0);
  iCurrentPos=0;
}

const LispChar* CachedStdUserInput::StartPtr()
{
  if (iBuffer.size() == 0)
    Peek();
  return &iBuffer[0];
}

std::size_t CachedStdUserInput::Position() const
{
  return iCurrentPos;
}
