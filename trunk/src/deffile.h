/** \file deffile.h deffiles, which speed up loading.
 *  This module adds support for loading files which specify
 *  which script file to look for when trying to use a specific
 *  function.
 */

#ifndef __deffile_h__
#define __deffile_h__

#include "lisphash.h"

/** LispDefFile represents one file that can be loaded just-in-time.
 */
class LispDefFile
{
public:
    LispDefFile(LispStringPtr aFile);
    ~LispDefFile();
    void SetLoaded();
    inline LispBoolean IsLoaded();
    inline LispStringPtr FileName();

    LispStringPtr iFileName;
    LispBoolean   iIsLoaded;
};

/** LispDefFiles maintains an array of files that can be defloaded.
 * When the user invokes a DefLoad on a file, an entry is added to the
 * array of deffiles in the LispEnvironment class. When the function
 * is called, and there is no body of rules defined for this function,
 * the engine looks up the correct file to load from this associated
 * has class.
 */
class LispEnvironment;
class LispDefFiles : public LispAssociatedHash<LispDefFile>
{
public:
    LispDefFile* File(LispStringPtr aFileName);
};

void LoadDefFile(LispEnvironment& aEnvironment, LispStringPtr aFileName);

#include "deffile.inl"

#endif
