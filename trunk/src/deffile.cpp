
#include "yacasprivate.h"
#include "deffile.h"
#include "lispuserfunc.h"
#include "standard.h"
#include "lispio.h"
#include "platfileio.h"
#include "lispenvironment.h"
#include "tokenizer.h"
#include "stringio.h"

LispDefFile::LispDefFile(const LispDefFile& aOther) : iFileName(aOther.iFileName),iIsLoaded(aOther.iIsLoaded)
{
//  iFileName = (aOther.iFileName);
//  iIsLoaded = aOther.iIsLoaded;
//printf("1... %s ",iFileName->c_str());
//printf("refcount = %d\n",(int)iFileName->iReferenceCount);
}

LispDefFile::LispDefFile(LispString * aFileName) : iFileName(aFileName),iIsLoaded(0)
{
//  iFileName = aFileName;
//printf("2... %s ",iFileName->c_str());
//printf("refcount = %d\n",(int)iFileName->iReferenceCount);
}


LispDefFile::~LispDefFile()
{
  iFileName = (NULL);
}

void LispDefFile::SetLoaded()
{
  iIsLoaded=1;
}

LispDefFile* LispDefFiles::File(LispString * aFileName)
{
  // Create a new entry
  LispDefFile* file = LookUp(aFileName);
  if (!file)
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

  LispString * eof = aEnvironment.HashTable().LookUp("EndOfFile");
  LispString * end = aEnvironment.HashTable().LookUp("}");
  LispBoolean endoffile = LispFalse;

  LispTokenizer tok;

  while (!endoffile)
  {
    // Read expression
    LispString * token = tok.NextToken(*aEnvironment.CurrentInput(),
                          aEnvironment.HashTable());

    // Check for end of file
    if (token == eof || token == end)
    {
      endoffile = LispTrue;
    }
    // Else evaluate
    else
    {
      LispString * str = token;
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



void LoadDefFile(LispEnvironment& aEnvironment, LispString * aFileName)
{
  LISPASSERT(aFileName!=NULL);

  LispString flatfile;
  InternalUnstringify(flatfile, aFileName);
  flatfile[flatfile.Size()-1]='.';
  flatfile.Append('d');
  flatfile.Append('e');
  flatfile.Append('f');
  flatfile.Append('\0');

  LispDefFile* def = aEnvironment.DefFiles().File(aFileName);
  LispString * contents = aEnvironment.FindCachedFile(flatfile.c_str());
  LispString * hashedname = aEnvironment.HashTable().LookUp(flatfile.c_str());

  InputStatus oldstatus = aEnvironment.iInputStatus;
  aEnvironment.iInputStatus.SetTo(hashedname->c_str());

  if (contents)
  {
    StringInput newInput(*contents,aEnvironment.iInputStatus);
    DoLoadDefFile(aEnvironment, &newInput,def);
    delete contents;
  }
  else
  {
    LispLocalFile localFP(aEnvironment, hashedname->c_str(),LispTrue,
                          aEnvironment.iInputDirectories);
    Check(localFP.iOpened != 0, KLispErrFileNotFound);
    FILEINPUT newInput(localFP,aEnvironment.iInputStatus);
    DoLoadDefFile(aEnvironment, &newInput,def);
  }
  aEnvironment.iInputStatus.RestoreFrom(oldstatus);
}




