

#include <stdio.h>

#include <string.h>

#include "yacasprivate.h"
#include "stdfileio.h"





InputStatus::~InputStatus()

{

}





CachedStdUserInput::CachedStdUserInput(InputStatus& aStatus) : LispInput(aStatus)

{

}



LispChar CachedStdUserInput::Next()

{

  return '\0';

}



LispChar CachedStdUserInput::Peek()

{

  return '\0';

}



LispBoolean CachedStdUserInput::EndOfStream()

{

  return LispTrue;

}



LispCharPtr CachedStdUserInput::StartPtr()

{

  return NULL;

}

LispInt CachedStdUserInput::Position()

{

  return 0;

}





StdFileOutput::StdFileOutput(LispLocalFile& aFile) : iFile(aFile.iFile) { }





void StdFileOutput::PutChar(LispChar aChar)

{

  TBuf<2> buf = _L(" ");

  buf[0] = aChar;

  iFile.Write(buf);

}















StdFileInput::~StdFileInput()

{

    PlatFree(iBuffer);

}



StdFileInput::StdFileInput(LispLocalFile& aFile,InputStatus& aStatus)

: LispInput(aStatus),iFile(aFile.iFile)

{

    // Get size of file



    iNrBytes=0;

    iFile.Seek(ESeekEnd,iNrBytes);

    TInt fp=0;

    iFile.Seek(ESeekStart,fp);

    

    // Read in the full buffer

    iBuffer = PlatAlloc(iNrBytes);

    Check(iBuffer!=NULL,KLispErrNotEnoughMemory);

    iCurrentPos = 0;



    TPtr p((unsigned char *)iBuffer,iNrBytes,iNrBytes);

    iFile.Read(p);

};



LispChar StdFileInput::Next()

{

    LISPASSERT(iCurrentPos < iNrBytes);

    return iBuffer[iCurrentPos++];

}



LispChar StdFileInput::Peek()

{

    LISPASSERT(iCurrentPos < iNrBytes);

    return iBuffer[iCurrentPos];

}



LispBoolean StdFileInput::EndOfStream()

{

    return (iCurrentPos >= iNrBytes);

}



LispCharPtr StdFileInput::StartPtr()

{

  return iBuffer;

}



LispInt StdFileInput::Position()

{

  return iCurrentPos;

}



LispLocalFile::LispLocalFile(LispEnvironment& aEnvironment,

                             LispCharPtr aFileName, LispBoolean aRead,

                             InputDirectories& aInputDirectories)

: iEnvironment(aEnvironment)

{

  TInt err=KErrNone;

    fs.Connect();



    if (aRead)

    {

      err = iFile.Open(fs,_L(aFileName),EFileRead);

      LispInt i=0;

      while (err != KErrNone && i<aInputDirectories.NrItems())

      {

        TFileName filename;

        filename = _L(aInputDirectories[i]->String());

        filename.Append(_L(aFileName));

        err = iFile.Open(fs,filename,EFileRead);

        i++;

      }

    }

    else

      err=iFile.Open(fs,_L(aFileName),EFileWrite);

    iOpened = 1;

    if (err != KErrNone)

      iOpened = 0;

    

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

    iFile.Close();

}







void InternalFindFile(LispCharPtr aFileName, InputDirectories& aInputDirectories,

                     LispCharPtr aFoundFile)

{
  aFoundFile[0] = '\0';
}

