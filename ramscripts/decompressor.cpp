
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

    int i;

    printf("%d %d\n",10,files.FindFile(files.Name(10)));
    printf("%d %d\n",20,files.FindFile(files.Name(20)));
    printf("%d %d\n",30,files.FindFile(files.Name(30)));
    printf("%d %d\n",-1,files.FindFile("NonExistentFile"));

    for (i=0;i<files.NrFiles();i++)
    {
        LispCharPtr name = files.Name(i);
        unsigned char buf[256];
        sprintf((char*)buf,"%s%s",argv[2],name);

        LispCharPtr expanded = files.Contents(i);
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
    
    unsigned char* ptr=fullbuf;
    int nrfiles   = getint(ptr);
    int indexsize = getint(ptr);
    unsigned char* indexbuf = ptr;
    unsigned char* indptr = indexbuf;
    unsigned char *endptr = indexbuf+indexsize;
    while (indptr<endptr)
    {
//        printf("from start %d\n",indexbuf-fullbuf);
//        printf("from end %d\n",endptr-indptr);
        int offset   = getint(indptr);
        int origsize = getint(indptr);
        int compressedsize = getint(indptr);

        /*
         printf("EXTRAIN: %d, %d, %d, %s\n",
               offset,
               origsize,
               compressedsize,
              indptr);
         */
//        printf("offset %d\n",offset);
//        printf("Inflating %s\n",indptr);

        unsigned char *expanded = (unsigned char*)PlatAlloc(origsize);

        lzo_uint new_len=origsize;
        int r = LZO_E_OK-1;
        if (lzo_compress)
        {
            r = lzo1x_decompress((unsigned char*)&fullbuf[offset],compressedsize,(unsigned char*)expanded,&new_len,NULL);
        }
        else
        {
            if (compressedsize != origsize)
            {
                printf("internal error: expected %d == %d\n",
                       compressedsize, origsize);
                exit(0);
            }
            r = LZO_E_OK;

            
            memcpy(expanded,&fullbuf[offset],origsize);

//            printf("uncompressed\n");
//            if (strstr(indptr,"editvi"))
//            {
//                printf("%s",expanded);
//                getchar();
//            }

        }
//        printf("%d %d %d\n",new_len, origsize,compressedsize);
//        printf("%s",expanded);
        if (r == LZO_E_OK)
        {
            char name[256];
            sprintf(name,"%s%s",argv[2],indptr);

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


            FILE*fout = fopen(name,"wb");
            if (fout)
            {
                printf("Inflating %d:\t %d: %s\n",compressedsize,origsize,name);
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
		/* this should NEVER happen */
		printf("internal error - decompression failed: %d\n", r);
		return 1;
	}
        
        PlatFree(expanded);
        indptr+=strlen((char*)indptr)+1;
    }
    
    fclose(fin);
    PlatFree(fullbuf);
    return 0;
}
