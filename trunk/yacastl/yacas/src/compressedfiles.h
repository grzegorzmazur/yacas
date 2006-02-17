
#ifndef __compressedfiles_h__
#define __compressedfiles_h__

#include "lisptype.h"

class CompressedFiles
{
public:
    CompressedFiles(unsigned char * aBuffer, LispInt aFullSize, LispInt aCompressed);
    ~CompressedFiles();
    LispInt FindFile(LispChar * aName);
    LispChar * Name(LispInt aIndex);
    LispChar * Contents(LispInt aIndex);
    inline LispInt NrFiles() const {return iNrFiles;}
    void Sizes(LispInt& aOriginalSize, LispInt& aCompressedSize, LispInt aIndex);
    inline LispInt IsValid() const {return iIsValid;}
protected:
    LispInt GetInt(unsigned char*&indptr);
    
private:
    unsigned char * iFullBuffer;
    LispInt iCompressed;
    LispInt iFullSize;
    unsigned char * *iIndex;
    LispInt iNrFiles;
    LispInt iIndexSize;
    LispInt iIsValid;
};

#endif // __compressedfiles_h__
