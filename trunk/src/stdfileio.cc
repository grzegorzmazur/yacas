

#include "stdfileio.h"

// For lack of better place to put it...
InputStatus::~InputStatus()
{
	if(iLineString != '\0')
		PlatFree(iLineString);
}

// TODO: use a LispLocalFile?  Problem:  would have to pass in
// a LispEnvironment address, when we don't need it for anything
// else
LispCharPtr InputStatus::Line(InputDirectories& aInputDirectories)
{
    FILE *iFile;
    LispChar c;
    LispInt iFileLine = 0, i = 0;
    LispUnsLong iFileSize1, iFileSize2;

    iFile = fopen(iFileName,"r");
    while(iFile == 0 && i < aInputDirectories.NrItems())
    {
        LispChar othername[1024]; //TODO
        strcpy(othername,aInputDirectories[i]->String());
        strcat(othername,iFileName);
        iFile = fopen(othername,"r");
        i++;
    }

	if(iFile == 0) {
		iLineString = PlatAlloc(1*sizeof(LispChar));
		iLineString[0] = '\0';
		return iLineString;
	}

	// move to the error
	while(iFileLine < iLineNumber-1 && !feof(iFile)) {
		c=fgetc(iFile);
		if (c == '\n')
			iFileLine++;
	}

	// Start of line
	iFileSize1 = ftell(iFile);

	while(iFileLine < iLineNumber && !feof(iFile)) {
		c=fgetc(iFile);
		if (c == '\n')
			iFileLine++;
	}

	// End of line
	iFileSize2 = ftell(iFile);
	iLineString = PlatAlloc((int)(iFileSize2-iFileSize1+1));

	fseek(iFile,iFileSize1,SEEK_SET);

	for(i = 0; i < iFileSize2-iFileSize1; i++)
		iLineString[i] = fgetc(iFile);
	iLineString[i] = '\0';

	return iLineString;
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
};

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


void InternalFindFile(LispCharPtr aFileName, InputDirectories& aInputDirectories,
                     LispCharPtr aFoundFile)
{
    strcpy(aFoundFile,aFileName);
    FILE* file = fopen(aFileName,"r");
    LispInt i=0;
    while (file == NULL && i<aInputDirectories.NrItems())
    {
        strcpy(aFoundFile,aInputDirectories[i]->String());
        strcat(aFoundFile,aFileName);
        file = fopen(aFoundFile,"r");
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
    if (aRead)
    {
        LispChar othername[1024];//TODO
        strcpy(othername,aFileName);
        iFile = fopen(aFileName,"r");
        LispInt i=0;
        while (iFile == NULL && i<aInputDirectories.NrItems())
        {
            strcpy(othername,aInputDirectories[i]->String());
            strcat(othername,aFileName);
            iFile = fopen(othername,"r");
            i++;
        }
    }
    else
        iFile = fopen(aFileName,"w");

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
};
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


