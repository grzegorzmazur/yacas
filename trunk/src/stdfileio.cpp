

#include "yacasprivate.h"
#include "stdfileio.h"

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




// For lack of better place to put it...
InputStatus::~InputStatus()
{
}


StdFileInput::StdFileInput(FILE* aFile,InputStatus& aStatus)
    : LispInput(aStatus)
{
    iFile = aFile;
}
StdFileInput::StdFileInput(LispLocalFile& aFile,InputStatus& aStatus)
    : LispInput(aStatus)
{
    iFile = aFile.iFile;
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

LispBoolean StdFileInput::EndOfStream() 
{
    return feof(iFile);
}

LispCharPtr StdFileInput::StartPtr()
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

CachedStdFileInput::CachedStdFileInput(LispLocalFile& aFile,InputStatus& aStatus) : StdFileInput(aFile,aStatus)
{
    // Get size of file
    fseek(iFile,0,SEEK_END);
    iNrBytes = ftell(iFile);
    fseek(iFile,0,SEEK_SET);
    // Read in the full buffer
    iBuffer = PlatAlloc(iNrBytes+1);
    Check(iBuffer!=NULL,KLispErrNotEnoughMemory);
    iCurrentPos = 0;
    fread(iBuffer,iNrBytes,1,iFile);
    iBuffer[iNrBytes] = '\0';
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

LispBoolean CachedStdFileInput::EndOfStream()
{
    return (iCurrentPos >= iNrBytes);
}

LispCharPtr CachedStdFileInput::StartPtr()
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


void InternalFindFile(LispCharPtr aFileName, InputDirectories& aInputDirectories,
                     LispCharPtr aFoundFile)
{
    strcpy(aFoundFile,aFileName);

    MapPathSeparators(aFoundFile);

    FILE* file = fopen(aFoundFile,"rb");
    LispInt i=0;
    while (file == NULL && i<aInputDirectories.NrItems())
    {
        strcpy(aFoundFile,aInputDirectories[i]->String());
        strcat(aFoundFile,aFileName);
        MapPathSeparators(aFoundFile);
        file = fopen(aFoundFile,"rb");
        i++;
    }
    if (file != NULL)
    {
        fclose(file);
    }
    else
    {
        aFoundFile[0] = '\0';
    }
}

LispLocalFile::LispLocalFile(LispEnvironment& aEnvironment,
                             LispCharPtr aFileName, LispBoolean aRead,
                             InputDirectories& aInputDirectories)
: iEnvironment(aEnvironment)
{
    LispChar othername[1024];//TODO
    if (aRead)
    {
        strcpy(othername,aFileName);
        MapPathSeparators(othername);

        iFile = fopen(othername,"rb");
        LispInt i=0;
        while (iFile == NULL && i<aInputDirectories.NrItems())
        {
            strcpy(othername,aInputDirectories[i]->String());
            strcat(othername,aFileName);
            MapPathSeparators(othername);
            iFile = fopen(othername,"rb");
            i++;
        }
    }
    else
    {
      strcpy(othername,aFileName);
      MapPathSeparators(othername);
      iFile = fopen(othername,"w");
    }

    if (iFile == NULL)
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
StdUserInput(aStatus)
{
//printf("CachedStdUserInput:construct\n");
    Rewind();
}
LispChar CachedStdUserInput::Next()
{
//printf("CachedStdUserInput:Next\n");
    LispChar c = Peek();
    iCurrentPos++;
    printf("%c",c);
    return c;
}

LispChar CachedStdUserInput::Peek()
{
    if (iCurrentPos == iBuffer.NrItems())
    {
        iBuffer.Append(fgetc(iFile));
    }
    return iBuffer[iCurrentPos];
}

LispBoolean CachedStdUserInput::EndOfStream()
{
    return LispFalse;
}

void CachedStdUserInput::Rewind()
{
    // Make sure there is a buffer to point to.
    iBuffer.GrowTo(10);
    iBuffer.SetNrItems(0);
    iCurrentPos=0;
}

LispCharPtr CachedStdUserInput::StartPtr()
{
    if (iBuffer.NrItems() == 0)
        Peek();
    return &iBuffer[0];
}

LispInt CachedStdUserInput::Position()
{
    return iCurrentPos;
}


