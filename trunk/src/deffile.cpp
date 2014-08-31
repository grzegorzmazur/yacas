
#include "yacas/yacasprivate.h"
#include "yacas/deffile.h"
#include "yacas/lispuserfunc.h"
#include "yacas/standard.h"
#include "yacas/lispio.h"
#include "yacas/platfileio.h"
#include "yacas/lispenvironment.h"
#include "yacas/tokenizer.h"
#include "yacas/stringio.h"

LispDefFile::LispDefFile(LispString* aFileName):
    iFileName(aFileName),
    iIsLoaded(false)
{
}

void LispDefFile::SetLoaded()
{
  iIsLoaded = true;
}

LispDefFile* LispDefFiles::File(LispString* aFileName)
{
    auto i = _map.find(aFileName);

    if (i == _map.end())
        i = _map.emplace(aFileName, aFileName).first;

    return &i->second;
}

static void DoLoadDefFile(
    LispEnvironment& aEnvironment,
    LispInput* aInput,
    LispDefFile* def)
{
  LispLocalInput localInput(aEnvironment, aInput);

  LispString * eof = aEnvironment.HashTable().LookUp("EndOfFile");
  LispString * end = aEnvironment.HashTable().LookUp("}");
  bool endoffile = false;

  LispTokenizer tok;

  while (!endoffile)
  {
    // Read expression
    LispString * token = tok.NextToken(*aEnvironment.CurrentInput(),
                          aEnvironment.HashTable());

    // Check for end of file
    if (token == eof || token == end)
    {
      endoffile = true;
    }
    // Else evaluate
    else
    {
      LispString * str = token;
      LispMultiUserFunction* multiUser = aEnvironment.MultiUserFunction(str);
      if (multiUser->iFileToOpen!=nullptr)
      {
        aEnvironment.CurrentOutput()->Write("[");
        aEnvironment.CurrentOutput()->Write(&(*str)[0]);
        aEnvironment.CurrentOutput()->Write("]\n");
        if (multiUser->iFileToOpen)
            throw LispErrDefFileAlreadyChosen();
      }
      multiUser->iFileToOpen = def;
    }
  }
}

void LoadDefFile(LispEnvironment& aEnvironment, LispString * aFileName)
{
  assert(aFileName!=nullptr);

  LispString flatfile;
  InternalUnstringify(flatfile, *aFileName);
  flatfile.append(".def");
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
    LispLocalFile localFP(aEnvironment, hashedname->c_str(),true,
                          aEnvironment.iInputDirectories);
    if (!localFP.stream.is_open())
        throw LispErrFileNotFound();

    CachedStdFileInput newInput(localFP,aEnvironment.iInputStatus);
    DoLoadDefFile(aEnvironment, &newInput,def);
  }
  aEnvironment.iInputStatus.RestoreFrom(oldstatus);
}
