#include "yacas/deffile.h"

#include "yacas/lispenvironment.h"
#include "yacas/lisperror.h"
#include "yacas/lispio.h"
#include "yacas/lispuserfunc.h"
#include "yacas/platfileio.h"
#include "yacas/standard.h"
#include "yacas/stringio.h"
#include "yacas/tokenizer.h"

LispDefFile::LispDefFile(const std::string& aFileName) :
    iFileName(aFileName),
    iIsLoaded(false)
{
}

void LispDefFile::SetLoaded()
{
    iIsLoaded = true;
}

LispDefFile* LispDefFiles::File(const std::string& aFileName)
{
    auto i = _map.find(aFileName);

    if (i == _map.end())
        i = _map.emplace(aFileName, aFileName).first;

    return &i->second;
}

static void DoLoadDefFile(LispEnvironment& aEnvironment,
                          LispInput* aInput,
                          LispDefFile* def)
{
    LispLocalInput localInput(aEnvironment, aInput);

    const LispString* eof = aEnvironment.iEndOfFile->String();
    const LispString* end = aEnvironment.iListClose->String();

    bool endoffile = false;

    LispTokenizer tok;

    while (!endoffile) {
        // Read expression
        const LispString* token = aEnvironment.HashTable().LookUp(
            tok.NextToken(*aEnvironment.CurrentInput()));

        // Check for end of file
        if (token == eof || token == end) {
            endoffile = true;
        }
        // Else evaluate
        else {
            LispMultiUserFunction* multiUser =
                aEnvironment.MultiUserFunction(token);

            if (multiUser->iFileToOpen != nullptr) {
                aEnvironment.CurrentOutput() << '[' << *token << "]\n";
                if (multiUser->iFileToOpen)
                    throw LispErrDefFileAlreadyChosen();
            }

            multiUser->iFileToOpen = def;

            def->symbols.insert(token);

            aEnvironment.Protect(token);
        }
    }
}

void LoadDefFile(LispEnvironment& aEnvironment, const std::string& aFileName)
{
    const std::string flatfile = InternalUnstringify(aFileName) + ".def";
    LispDefFile* def = aEnvironment.DefFiles().File(aFileName);

    InputStatus oldstatus = aEnvironment.iInputStatus;
    aEnvironment.iInputStatus.SetTo(flatfile);

    LispLocalFile localFP(
        aEnvironment, flatfile, true, aEnvironment.iInputDirectories);
    if (!localFP.stream.is_open())
        throw LispErrFileNotFound();

    StdFileInput newInput(localFP, aEnvironment.iInputStatus);
    DoLoadDefFile(aEnvironment, &newInput, def);

    aEnvironment.iInputStatus.RestoreFrom(oldstatus);
}
