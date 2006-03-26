
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lisptype.h"
#include "filescanner.h"
#include "minilzo.h"
#include "stubs.h"
#include "compressedfiles.h"

int lzo_compress=1;

int getint(unsigned char*&indptr)
{
    int c0,c1,c2,c3;
    c0 = *indptr++;
    c1 = *indptr++;
    c2 = *indptr++;
    c3 = *indptr++;
    return ((((((c3<<8)+c2)<<8)+c1)<<8)+c0);
}

void MakeSureDirExists(char *name)
{
    {
        char *ptr = name;
        while (*ptr)
        {
            if (*ptr == '/')
            {
                *ptr = '\0';

                DIR *dp;
                dp = opendir(name);
                if (dp == NULL)
                {
                    char dum[256];
                    sprintf(dum,"mkdir %s",name);
                    system(dum);
                }
                else
                {
                    closedir(dp);
                }
                *ptr = '/';
            }
            ++ptr;
        }
    }
}

int main(int argc, char** argv)
{
    if (argc<4)
    {
        printf("usage: decompressor flags outdir infile\n");
        printf("       l - no lzo compression\n");
        return 0;
    }
    if (strstr(argv[1],"l")) lzo_compress=0;

    FILE*fin = fopen(argv[3],"rb");
    if (!fin)
    {
        printf("Error, could not open file %s\n",argv[3]);
        exit(0);
    }
    fseek(fin,0,SEEK_END);
    int fullsize = ftell(fin);
    fseek(fin,0,SEEK_SET);
    unsigned char* fullbuf = (unsigned char*)PlatAlloc(fullsize);
    fread(fullbuf,1,fullsize,fin);
    fclose(fin);

    CompressedFiles files(fullbuf,fullsize,lzo_compress);
    if (!files.IsValid())
    {
        printf("Error, %s is not a valid archive file.\n",argv[3]);
        exit(0);
    }
    int i;

    printf("%d %d\n",10,files.FindFile(files.Name(10)));
    printf("%d %d\n",20,files.FindFile(files.Name(20)));
    printf("%d %d\n",30,files.FindFile(files.Name(30)));
    printf("%d %d\n",-1,files.FindFile("NonExistentFile"));

    for (i=0;i<files.NrFiles();i++)
    {
        LispChar * name = files.Name(i);
        unsigned char buf[256];
        sprintf((char*)buf,"%s%s",argv[2],name);

        LispChar * expanded = files.Contents(i);
        if (expanded)
        {
            MakeSureDirExists((char*)buf);
            FILE*fout = fopen((char*)buf,"wb");
            if (fout)
            {
                int compressedsize,origsize;
                files.Sizes(origsize, compressedsize, i);
                printf("Inflating %d:\t %d: %s\n",compressedsize,origsize,buf);
                fwrite(expanded,1,origsize,fout);
                fclose(fout);
            }
            else
            {
                printf("Error, could not write to file %s\n",name);
                exit(0);
            }
        }
        else
        {
            printf("Error, could not decompress file %s\n",name);
            exit(0);
        }
        PlatFree(expanded);
    }
    printf("finished\n");
    return 0;
}
