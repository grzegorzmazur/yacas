
#include "yacasprivate.h"
#include "deffile.h"
#include "lispuserfunc.h"
#include "standard.h"
#include "lispio.h"
#include "platfileio.h"
#include "lispenvironment.h"
#include "tokenizer.h"
#include "stringio.h"


LispDefFile::LispDefFile(const LispDefFile& aOther)
{
    iFileName.Set(aOther.iFileName());
    iIsLoaded=aOther.iIsLoaded;
}

LispDefFile::LispDefFile(LispStringPtr aFileName)
{
    iFileName.Set(aFileName);
//TODO remove?    aFileName->IncreaseRefCount();
    iIsLoaded=0;
}


LispDefFile::~LispDefFile()
{
    iFileName.Set(NULL);
//TODO remove?    iFileName->DecreaseRefCount();
}

void LispDefFile::SetLoaded()
{
    iIsLoaded=1;
}

LispDefFile* LispDefFiles::File(LispStringPtr aFileName)
{
    // Create a new entry
    LispDefFile* file = LookUp(aFileName);
    if (file == NULL)
    {
        LispDefFile newfile(aFileName);
        // Add the new entry to the hash table
        SetAssociation(newfile, aFileName);
        file = LookUp(aFileName);
    }
    return file;
}

static void DoLoadDefFile(LispEnvironment& aEnvironment, LispInput* aInput,
                          LispDefFile* def)
{
    LispLocalInput localInput(aEnvironment, aInput);

    LispStringPtr eof = aEnvironment.HashTable().LookUp("EndOfFile");
    LispStringPtr end = aEnvironment.HashTable().LookUp("}");
    LispBoolean endoffile = LispFalse;

    LispTokenizer tok;

    while (!endoffile)
    {
        // Read expression
        LispStringPtr token;
        token = tok.NextToken(*aEnvironment.CurrentInput(),
                              aEnvironment.HashTable());

        // Check for end of file
        if (token == eof || token == end)
        {
            endoffile = LispTrue;
        }
        // Else evaluate
        else
        {
            LispStringPtr str = token;
            LispMultiUserFunction* multiUser = aEnvironment.MultiUserFunction(str);
            if (multiUser->iFileToOpen!=NULL)
            {
                aEnvironment.CurrentOutput()->Write("[");
                aEnvironment.CurrentOutput()->Write(&(*str)[0]);
                aEnvironment.CurrentOutput()->Write("]\n");
                Check(multiUser->iFileToOpen==NULL,KLispErrDefFileAlreadyChosen);
            }
            multiUser->iFileToOpen = def;
        }
    }
}



void LoadDefFile(LispEnvironment& aEnvironment, LispStringPtr aFileName)
{
    LISPASSERT(aFileName!=NULL);

    LispString flatfile;
    InternalUnstringify(flatfile, aFileName);
    flatfile[flatfile.NrItems()-1]='.';
    flatfile.Append('d');
    flatfile.Append('e');
    flatfile.Append('f');
    flatfile.Append('\0');

    LispDefFile* def = aEnvironment.DefFiles().File(aFileName);
    
    LispStringPtr hashedname = aEnvironment.HashTable().LookUp(flatfile.String());
    LispRamFile* ramFile=aEnvironment.iRamDisk.LookUp(hashedname);

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(hashedname->String());

    if (ramFile != NULL)
    {
        StringInput newInput(*(ramFile->Contents()),aEnvironment.iInputStatus);
        DoLoadDefFile(aEnvironment, &newInput,def);
    }
    else
    {
        LispLocalFile localFP(aEnvironment, flatfile.String(),LispTrue,
                              aEnvironment.iInputDirectories);
        Check(localFP.iOpened != 0, KLispErrFileNotFound);
        FILEINPUT newInput(localFP,aEnvironment.iInputStatus);
        DoLoadDefFile(aEnvironment, &newInput,def);
    }
    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
}




