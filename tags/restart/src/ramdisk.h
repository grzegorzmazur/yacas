
/** \file ramdisk.h
 *  LispRamDisk implements an in-memory file system by deriving from
 *  LispAssociatedHash.
 *
 * This LispAssociatedHash implementation associates a string (the file
 *  name) to another string (the contents of the file). As such it can
 *  be used as a file system. The LispRamFile can even be used to hard-code
 *  the standard math library scripts into the executable, by adding
 *  the strings to the LispRamFile at startup. This is platform-independent
 *  also.
 */

#ifndef __ramdisk_h__
#define __ramdisk_h__

#include "lisphash.h"
#include "lispstring.h"
#include "grower.h"

/** class representing the contents of a file. See LispRamDisk for
 *  more details.
 */
class LispRamFile
{
public:
    LispRamFile(LispCharPtr aFileContents)
        : iFileContents(aFileContents,LispTrue)
    {
    };
    LispRamFile(const LispRamFile& orig)
        : iFileContents(orig.iFileContents.String(),LispTrue)
        {
        }
    LispStringPtr Contents()
    {
        return &iFileContents;
    }
private:
    LispString iFileContents;
};

/** class maintaining a set of object instantiations of the
 *  class LispRamFile. As such it acts as an in-memory file system.
 */
class LispRamDisk : public LispAssociatedHash<LispRamFile>
{
};

#endif

