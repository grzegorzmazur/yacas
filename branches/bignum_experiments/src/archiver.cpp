
#include "yacasprivate.h"
#include "archiver.h"
#include "minilzo.c"

CCompressedArchive::CCompressedArchive(unsigned char * aBuffer, LispInt aFullSize, LispInt aCompressed)
: iFiles(aBuffer, aFullSize, aCompressed)
{
}


