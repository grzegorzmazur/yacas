
#include "yacasprivate.h"
#include "archiver.h"

#include <lzo/lzoconf.h>
#include <lzo/lzo1x.h>

CCompressedArchive::CCompressedArchive(unsigned char * aBuffer, LispInt aFullSize, LispInt aCompressed)
: iFiles(aBuffer, aFullSize, aCompressed)
{
}


