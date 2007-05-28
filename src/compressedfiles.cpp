
#include "yacasprivate.h"
#include "compressedfiles.h"
#include "lisptype.h"
#include "lispassert.h"
#include "stubs.h"
#include "standard.h"
#include "minilzo.h"





CompressedFiles::CompressedFiles(unsigned char * aBuffer, LispInt aFullSize, LispInt aCompressed)
: iFullBuffer(aBuffer),iCompressed(aCompressed),iFullSize(aFullSize),iIndex(NULL),iNrFiles(0),iIndexSize(0),iIsValid(LispFalse)
{
    if (!iFullBuffer) return;

    // the archive cannot possibly be less than 8 bytes
    if (iFullSize < 8) return;
 
    unsigned char * ptr=iFullBuffer;
    iNrFiles   = GetInt(ptr);
    iIndexSize = GetInt(ptr);

    // These have to be positive integers to make sense
    if (iNrFiles<=0) return;
    if (iIndexSize<=0) return;

    // The archive can not possibly be empty
    if (8+iIndexSize>=iFullSize) return;

    // 1000 is just an arbitrary size, to disallow this tool
    // from allocating too much memory
    if (iNrFiles>1000) return;
 
    iIndex = PlatAllocN<unsigned char *>(iNrFiles);
    if (!iIndex) return;

    {
        LispInt i;
        iIndex[0] = ptr;
        for (i=1;i<iNrFiles;i++)
        {
            LispInt offset         = GetInt(ptr);
            /* LispInt origsize = */ GetInt(ptr);
            LispInt compressedsize = GetInt(ptr);

            if (offset<=iIndexSize) return;
            if (offset+compressedsize > iFullSize) return;
 
            ptr+=PlatStrLen((LispChar *)ptr)+1;

            if ((ptr-iFullBuffer)>8+iIndexSize) return;

            iIndex[i] = ptr;
        }
    }
    iIsValid = LispTrue;
}

CompressedFiles::~CompressedFiles()
{
    PlatFree(iIndex);
    PlatFree(iFullBuffer);
}


LispInt CompressedFiles::GetInt(unsigned char*&indptr)
{
    int c0,c1,c2,c3;
    c0 = *indptr++;
    c1 = *indptr++;
    c2 = *indptr++;
    c3 = *indptr++;
    return ((((((c3<<8)+c2)<<8)+c1)<<8)+c0);
}

LispInt CompressedFiles::FindFile(LispChar * aName)
{
    LISPASSERT(IsValid());
    LispInt low=0, high=iNrFiles;
    LispInt mid;
    for(;;)
    {
        if (low>=high)
        {
            mid=-1;
            goto CONTINUE;
        }
        mid = (low+high)>>1;

        LispInt cmp = StrCompare(aName, Name(mid));
        if (cmp < 0)
        {
            high = mid;
        }
        else if (cmp > 0)
        {
            low = (++mid);
        }
        else
        {
            goto CONTINUE;
        }
    }
CONTINUE:
    return mid;
}


LispChar * CompressedFiles::Name(LispInt aIndex)
{
    LISPASSERT(IsValid());
    LISPASSERT(aIndex >= 0 && aIndex < iNrFiles);
    unsigned char * ptr=iIndex[aIndex];
    /* LispInt offset = */         GetInt(ptr);
    /* LispInt origsize = */       GetInt(ptr);
    /* LispInt compressedsize = */ GetInt(ptr);
    return (LispChar *)ptr;
}
void CompressedFiles::Sizes(LispInt& aOriginalSize, LispInt& aCompressedSize, LispInt aIndex)
{
    LISPASSERT(IsValid());
    LISPASSERT(aIndex >= 0 && aIndex < iNrFiles);
    unsigned char * ptr=iIndex[aIndex];
    /* LispInt offset = */ GetInt(ptr);
    aOriginalSize   = GetInt(ptr);
    aCompressedSize = GetInt(ptr);
}

LispChar * CompressedFiles::Contents(LispInt aIndex)
{
    LISPASSERT(IsValid());
    LISPASSERT(aIndex >= 0 && aIndex < iNrFiles);
    unsigned char * ptr=iIndex[aIndex];
    LispInt offset         = GetInt(ptr);
    LispInt origsize       = GetInt(ptr);
    LispInt compressedsize = GetInt(ptr);


    unsigned char * expanded = PlatAllocN<unsigned char>(origsize+1);

    lzo_uint new_len=origsize;
    int r = LZO_E_OK-1;
    if (iCompressed)
    {
        r = lzo1x_decompress(&iFullBuffer[offset],compressedsize,expanded,&new_len,NULL);
        if ((LispInt)new_len != (LispInt)origsize)
        {
            PlatFree(expanded);
            return NULL;
        }
    }
    else
    {
        if (compressedsize == origsize)
        {
            r = LZO_E_OK;
            PlatMemCopy(expanded,&iFullBuffer[offset],origsize);
        }
    }
    expanded[origsize] = '\0';
    if (r != LZO_E_OK)
    {
        PlatFree(expanded);
        expanded = NULL;
    }
    return (LispChar *)expanded;
}

