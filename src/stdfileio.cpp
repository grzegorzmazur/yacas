

#include "yacas/yacasprivate.h"
#include "yacas/stdfileio.h"

#ifdef WIN32
#define MAP_TO_WIN32_PATH_SEPARATOR
#endif // WIN32

//#define MAP_TO_WIN32_PATH_SEPARATOR

static void MapPathSeparators(char* filename)
{
//  printf("File name [%s]",filename);
//  char* ptr = filename;
#ifdef MAP_TO_WIN32_PATH_SEPARATOR
  while (*filename)
  {
    switch (*filename)
    {
      case '/': *filename = '\\';
      default: filename++;
    }
  }
#endif // MAP_TO_WIN32_PATH_SEPARATOR
//  printf("-> [%s]\n",ptr);
}

static void MapPathSeparators(std::string& filename)
{
#ifdef MAP_TO_WIN32_PATH_SEPARATOR
    for (std::size_t i = 0; i < filename.size(); ++i)
        if (filename[i] == '/')
            filename[i] = '\\';
#endif
}



// For lack of better place to put it...
InputStatus::~InputStatus()
{
}


StdFileInput::StdFileInput(FILE* aFile,InputStatus& aStatus)
    : LispInput(aStatus),  iFile(aFile)
{
}
StdFileInput::StdFileInput(LispLocalFile& aFile,InputStatus& aStatus)
    : LispInput(aStatus),iFile(aFile.iFile)
{
}



LispChar StdFileInput::Next()
{
  LispChar c;
  c=fgetc(iFile);
  if (c == '\n')
    iStatus.NextLine();
  return c;
}

LispChar StdFileInput::Peek()
{
  LispChar c = fgetc(iFile);
  ungetc(c,iFile);
  return c;
}
 
void StdFileInput::Rewind()
{
  fseek(iFile,0,SEEK_SET);
}

bool StdFileInput::EndOfStream()
{
  return feof(iFile);
}

const LispChar* StdFileInput::StartPtr()
{
  LISPASSERT(0);
  return NULL;
}
LispInt StdFileInput::Position()
{
    LISPASSERT(0);
  return 0;
}

void StdFileInput::SetPosition(LispInt aPosition)
{
  LISPASSERT(0);
}




StdFileOutput::StdFileOutput(FILE* aFile) : iFile(aFile) { }
StdFileOutput::StdFileOutput(LispLocalFile& aFile) : iFile(aFile.iFile) { }


void StdFileOutput::PutChar(LispChar aChar)
{
    fputc(aChar, iFile);
}







CachedStdFileInput::~CachedStdFileInput()
{
    PlatFree(iBuffer);
}

CachedStdFileInput::CachedStdFileInput(LispLocalFile& aFile,InputStatus& aStatus) : StdFileInput(aFile,aStatus),iBuffer(NULL),iCurrentPos(0),iNrBytes(0)
{
  // Get size of file
  fseek(iFile,0,SEEK_END);
  iNrBytes = ftell(iFile);
  fseek(iFile,0,SEEK_SET);
  // Read in the full buffer
  char * ptr = PlatAllocN<char>(iNrBytes+1);      // sizeof(char) == 1
  iBuffer = (LispChar *)ptr;                                        // sizeof(LispChar) == not so sure

  if (!ptr)
      throw LispErrNotEnoughMemory();

  const LispInt n = fread(ptr,iNrBytes,1,iFile);

  if (n != 1)
      throw LispErrReadingFile();

  ptr[iNrBytes] = '\0';
}

LispChar CachedStdFileInput::Next()
{
    LispChar c;
    LISPASSERT(iCurrentPos < iNrBytes);
    c = iBuffer[iCurrentPos++];

    if (c == '\n')
    {
        iStatus.NextLine();
    }
    return c;
}

LispChar CachedStdFileInput::Peek()
{
    LISPASSERT(iCurrentPos < iNrBytes);
    return iBuffer[iCurrentPos];
}


void CachedStdFileInput::Rewind()
{
  iCurrentPos = 0;
}

bool CachedStdFileInput::EndOfStream()
{
    return (iCurrentPos >= iNrBytes);
}

const LispChar* CachedStdFileInput::StartPtr()
{
    return iBuffer;
}
LispInt CachedStdFileInput::Position()
{
    return iCurrentPos;
}
void CachedStdFileInput::SetPosition(LispInt aPosition)
{
  LISPASSERT(aPosition>=0);
  LISPASSERT(aPosition<iNrBytes);
  iCurrentPos = aPosition;
}


// TODO: woof -- buffer overflow problems in here?
void InternalFindFile(const LispChar * aFileName, InputDirectories& aInputDirectories,
                      LispChar* aFoundFile)
{
    strcpy(aFoundFile,aFileName);

    MapPathSeparators(aFoundFile);

    FILE* file = fopen(aFoundFile,"rb");
    LispInt i=0;
    while (!file && i<aInputDirectories.Size())
    {
        strcpy(aFoundFile,aInputDirectories[i]->c_str());
        strcat(aFoundFile,aFileName);
        MapPathSeparators(aFoundFile);
        file = fopen(aFoundFile,"rb");
        i++;
    }
    if (file)
    {
        fclose(file);
    }
    else
    {
        aFoundFile[0] = '\0';
    }
}

LispLocalFile::LispLocalFile(LispEnvironment& aEnvironment,
                             const LispChar * aFileName, bool aRead,
                             InputDirectories& aInputDirectories)
  : iFile(NULL),iEnvironment(aEnvironment),iOpened(false)
{
  std::string othername;

  if (aRead)
  {
    othername = aFileName;
    MapPathSeparators(othername);

    iFile = fopen(othername.c_str(), "rb");
    LispInt i=0;
    while (!iFile && i<aInputDirectories.Size())
    {
      othername = aInputDirectories[i]->c_str();
      othername += aFileName;
      MapPathSeparators(othername);
      iFile = fopen(othername.c_str(), "rb");
      i++;
    }
  }
  else
  {
    othername = aFileName;
    MapPathSeparators(othername);
    iFile = fopen(othername.c_str(),"w");
  }

  if (!iFile)
    iOpened=0;
  else
    iOpened=1;

  SAFEPUSH(iEnvironment,*this);
}

//aRead is for opening in read mode (otherwise opened in write mode)
LispLocalFile::~LispLocalFile()
{
  SAFEPOP(iEnvironment);
  Delete();
}

void LispLocalFile::Delete()
{
  if (iFile)
    fclose(iFile);
  iFile = NULL;
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
  if (iCurrentPos == iBuffer.Size())
  {
    iBuffer.Append(fgetc(iFile));
  }
  return iBuffer[iCurrentPos];
}

bool CachedStdUserInput::EndOfStream()
{
  return false;
}

void CachedStdUserInput::Rewind()
{
  // Make sure there is a buffer to point to.
  iBuffer.ResizeTo(10);
  iBuffer.ResizeTo(0);
  iCurrentPos=0;
}

const LispChar* CachedStdUserInput::StartPtr()
{
  if (iBuffer.Size() == 0)
    Peek();
  return &iBuffer[0];
}

LispInt CachedStdUserInput::Position()
{
  return iCurrentPos;
}


