
#include "yacas/yacasprivate.h"
#include "yacas/deffile.h"
#include "yacas/lispuserfunc.h"
#include "yacas/standard.h"
#include "yacas/lispio.h"
#include "yacas/platfileio.h"
#include "yacas/lispenvironment.h"
#include "yacas/tokenizer.h"
#include "yacas/stringio.h"

LispDefFile::LispDefFile(const LispString* aFileName):
    iFileName(aFileName),
    iIsLoaded(false)
{
}

void LispDefFile::SetLoaded()
{
  iIsLoaded = true;
}

LispDefFile* LispDefFiles::File(const LispString* aFileName)
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

  const LispString* eof = aEnvironment.iEndOfFile->String();
  const LispString* end = aEnvironment.iListClose->String();

  bool endoffile = false;

  LispTokenizer tok;

  while (!endoffile)
  {
    // Read expression
    const LispString* token =
        tok.NextToken(*aEnvironment.CurrentInput(), aEnvironment.HashTable());

    // Check for end of file
    if (token == eof || token == end)
    {
      endoffile = true;
    }
    // Else evaluate
    else
    {
        LispMultiUserFunction* multiUser = aEnvironment.MultiUserFunction(token);
        if (multiUser->iFileToOpen != nullptr) {
            aEnvironment.CurrentOutput() << '[' << *token << "]\n";
            if (multiUser->iFileToOpen)
               throw LispErrDefFileAlreadyChosen();
        }
        multiUser->iFileToOpen = def;
    }
  }
}

void LoadDefFile(LispEnvironment& aEnvironment, const LispString* aFileName)
{
  assert(aFileName!=nullptr);

  const std::string flatfile = InternalUnstringify(*aFileName) + ".def";
  LispDefFile* def = aEnvironment.DefFiles().File(aFileName);
  const LispString* contents = aEnvironment.FindCachedFile(flatfile.c_str());
  const LispString* hashedname = aEnvironment.HashTable().LookUp(flatfile);

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
    LispLocalFile localFP(aEnvironment, *hashedname,true,
                          aEnvironment.iInputDirectories);
    if (!localFP.stream.is_open())
        throw LispErrFileNotFound();

    CachedStdFileInput newInput(localFP,aEnvironment.iInputStatus);
    DoLoadDefFile(aEnvironment, &newInput,def);
  }
  aEnvironment.iInputStatus.RestoreFrom(oldstatus);
}
