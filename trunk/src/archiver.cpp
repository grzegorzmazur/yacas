
#include "archiver.h"
#include "../ramscripts/compressedfiles.cpp"
#include "../ramscripts/minilzo.c"

CCompressedArchive::CCompressedArchive(unsigned char * aBuffer, LispInt aFullSize, LispInt aCompressed)
: iFiles(aBuffer, aFullSize, aCompressed)
{
}


