/** \file deffile.h deffiles, which speed up loading.
 *  This module adds support for loading files which specify
 *  which script file to look for when trying to use a specific
 *  function.
 */

#ifndef YACAS_DEFFILE_H
#define YACAS_DEFFILE_H

#include "yacas/lispstring.h"

#include <unordered_map>
#include <unordered_set>

/** LispDefFile represents one file that can be loaded just-in-time.
 */
class LispDefFile
{
public:
    LispDefFile(const std::string& aFile);

    void SetLoaded();
    bool IsLoaded() const;
    const std::string& FileName() const;

private:
    std::string iFileName;
    bool iIsLoaded;
public:
    std::unordered_set<const LispString*> symbols;
};

/** LispDefFiles maintains an array of files that can be defloaded.
 * When the user invokes a DefLoad on a file, an entry is added to the
 * array of deffiles in the LispEnvironment class. When the function
 * is called, and there is no body of rules defined for this function,
 * the engine looks up the correct file to load from this associated
 * has class.
 */
class LispDefFiles
{
public:
    LispDefFile* File(const std::string& aFileName);

private:
    std::unordered_map<std::string, LispDefFile> _map;
};

class LispEnvironment;

void LoadDefFile(LispEnvironment& aEnvironment, const std::string& aFileName);


inline
bool LispDefFile::IsLoaded() const
{
    return iIsLoaded;
}

inline
const std::string& LispDefFile::FileName() const
{
    return iFileName;
}

#endif
