
#include <stdio.h>
#include <stdlib.h>
#include "filescanner.h"
#include "minilzo.h"

//flags
int strip_script=1;
int lzo_compress=1;


int totalfiles=0;
int totalsize=0;

#define MAXFILES 500
char *filename[MAXFILES];
int  offset[MAXFILES];
int  origsize[MAXFILES];
int  compressedsize[MAXFILES];

//
// lzo specific
//
#define IN_LEN    (128*1024L)
#define OUT_LEN    (IN_LEN + IN_LEN / 64 + 16 + 3)
static lzo_byte in  [ IN_LEN ];
static lzo_byte out [ OUT_LEN ];
/* Work-memory needed for compression. Allocate memory in units
 * of `long' (instead of `char') to make sure it is properly aligned.
 */
#define HEAP_ALLOC(var,size) \
  long __LZO_MMODEL var [ ((size) + (sizeof(long) - 1)) / sizeof(long) ]
static HEAP_ALLOC(wrkmem,LZO1X_1_MEM_COMPRESS);


int CompressScript(char* contents,int &size)
{
    int r;
    lzo_uint in_len;
    lzo_uint out_len = size;
    lzo_uint new_len;
    in_len = size;
    r = lzo1x_1_compress((unsigned char*)contents,in_len,out,&out_len,wrkmem);
    if (r == LZO_E_OK)
    {
//        if (size>out_len)
        {
            size = out_len;
//            printf("(compressed %lu bytes into %lu bytes)",
//                   (long) in_len, (long) out_len);
            memcpy(contents,out,size);
        }
//        else
//        {
//            printf("(Not compressed)");
//        }
        return 1;
    }
    else
    {
        /* this should NEVER happen */
        printf("internal error - compression failed: %d\n", r);
    }
    return 0;
}


void StripScript(char *contents,int &stripsize)
{
    int src=0,trg=0;
    char c;
    while (src<stripsize)
    {
        c = contents[src++];
        switch (c)
        {
        case '\"':
          contents[trg++] = c;
          while (contents[src] != '\"')
          {
            if (contents[src] == '\\')
              contents[trg++] = contents[src++];
            contents[trg++] = contents[src++];
          }
          c = contents[src++];
          goto NORMAL;
          break;
        case ' ':
        case '\t':
            while (contents[src] == ' ' || contents[src] == '\t') src++;
            goto NORMAL;
        case '\n':
            if (trg>0 && contents[trg-1] == '\n')
                break;
            goto NORMAL;
        case '/':
            {
                char c2 = contents[src++];
                if (c2 == '/')
                {
                    while (c!='\n') c = contents[src++];
                    break;
                }
                else if (c2 == '*')
                {
                    c = contents[src++];
                    c2 = contents[src++];
                    for (;;)
                    {
                        if (c == '*' && c2 == '/')
                            break;
                        c = c2;
                        c2 = contents[src++];
                    }
                    break;
                }
                else
                {
                    src--;
                    goto NORMAL;
                }
            }
        default:
        NORMAL:
            contents[trg++] = c;
        }
    }
    stripsize = trg;
}


void DoFile(char* base,char* file)
{
    filename[totalfiles] = strdup(file);
    {
      char* ptr = filename[totalfiles];
      while (*ptr)
      {
        if (*ptr == '\\')
          *ptr = '/';
        ptr++;
      }
    }
    totalfiles++;
}
void PostProcessFile(FILE* tmpfilef,char* base,int index)
{
    char buf[256];
    sprintf(buf,"%s%s",base,filename[index]);
    FILE *f=fopen(buf,"rb");
    if (f)
    {
        fseek(f,0,SEEK_END);
        int stripsize = ftell(f);
        fseek(f,0,SEEK_SET);
        char *contents = (char*)malloc(stripsize+10);
        fread(contents,1,stripsize,f);
        fclose(f);

        printf("%d:\t",stripsize);

        if (strip_script)
        {
            StripScript(contents,stripsize);
            printf("%d:\t",stripsize);
        }
        origsize[index] = stripsize;
        if (lzo_compress)
        {
            if (!CompressScript(contents,stripsize))
            {
                printf("Warning: failed to compress %s\n",buf);
                goto BROKEN;
            }
        }
        fwrite(contents,1,stripsize,tmpfilef);
        offset[index] = totalsize;
        compressedsize[index] = stripsize;
 
        totalsize+=stripsize;
        printf("%d:\t",stripsize);
        printf("%s\n",filename[index]);

    BROKEN:
        free(contents);
    }
    else
    {
        printf("Warning: could not open file %s\n",buf);
    }
}
void WalkDirs(char* base,char* dir)
{
    CFileScanner scanner;
    CFileNode* node = scanner.First(base,dir);
    while (node)
    {
        if (node->IsDirectory())
        {
            if (!strstr(node->FullName(),"CVS"))
            {
                WalkDirs(base,node->FullName());
            }
        }
        else
        {
            if (!strstr(node->FullName(),".#"))
            if (!strstr(node->FullName(),"~"))
            if (!strstr(node->FullName(),"Makefile"))
            if (!strstr(node->FullName(),"old"))
            if (!strstr(node->FullName(),"README"))
            if (!strstr(node->FullName(),"maketest"))
            {
                DoFile(base,node->FullName());
            }
        }
        node = scanner.Next();
    }
}

int fncompare(const void* e1, const void* e2)
{
    return strcmp(*((const char**)e1),*((const char**)e2));
}

void  putint(char*&indptr,int n)
{
    *indptr++ = n&0xff; n>>=8;
    *indptr++ = n&0xff; n>>=8;
    *indptr++ = n&0xff; n>>=8;
    *indptr++ = n&0xff;
}
void  putint(FILE*fout,int n)
{
    fputc(n&0xff,fout); n>>=8;
    fputc(n&0xff,fout); n>>=8;
    fputc(n&0xff,fout); n>>=8;
    fputc(n&0xff,fout);
}

void makeindex(char* indexbuf,char*&indptr,int offs)
{
    indptr = indexbuf;
    int i;
    for (i=0;i<totalfiles;i++)
    {
        putint(indptr,offset[i]+offs);
        putint(indptr,origsize[i]);
        putint(indptr,compressedsize[i]);
        strcpy(indptr,filename[i]);
        indptr+=strlen(filename[i])+1;

        /*
         printf("EXTRAOUT: %d, %d, %d, %s\n",
               offset[i]+offs,
               origsize[i],
               compressedsize[i],
               filename[i]);
               */
    }
}

int main(int argc, char** argv)
{
    if (argc<4)
    {
        printf("usage: compressor flags indir outfile\n");
        printf("flags: c - no script stripping\n");
        printf("       l - no lzo compression\n");
        return 0;
    }

    if (strstr(argv[1],"c")) strip_script=0;
    if (strstr(argv[1],"l")) lzo_compress=0;
    totalfiles=0;
    totalsize=0;

    if (lzo_init() != LZO_E_OK)
    {
        printf("lzo_init() failed !!!\n");
        return 3;
    }
    WalkDirs(argv[2],"");
    qsort(filename,totalfiles,sizeof(char*),fncompare);
    FILE* tmpfilef=fopen("tmpfile.tmp","wb");

    {
        int i;
        for (i=0;i<totalfiles;i++)
        {
            PostProcessFile(tmpfilef,argv[2],i);
        }
    }
    fclose(tmpfilef);


 
    printf("%d files, %d bytes packed.\n",totalfiles,totalsize);

    printf("Writing to file %s...\n",argv[3]);
    FILE* fout = fopen(argv[3],"wb");

    if (fout)
    {
        char indexbuf[65536];
        char* indptr;
        makeindex(indexbuf,indptr,0);
        int indexlength = indptr-indexbuf;
        makeindex(indexbuf,indptr,2*sizeof(int)+indexlength);
  printf("%d bytes uncompressed index\n",2*sizeof(int)+indexlength);
        putint(fout,totalfiles);
        putint(fout,indexlength);
        fwrite(indexbuf,1,indexlength,fout);
        {
            FILE* tmpfilef=fopen("tmpfile.tmp","rb");
            fseek(tmpfilef,0,SEEK_END);
            int size=ftell(tmpfilef);
            fseek(tmpfilef,0,SEEK_SET);
            char*buf = (char*)malloc(size);
            fread(buf,1,size,tmpfilef);
            fclose(tmpfilef);
            fwrite(buf,1,size,fout);
            free(buf);
        }

 
//        putint(fout,totalfiles);
    }
    {
        int i;
        for (i=0;i<totalfiles;i++)
        {
            free(filename[i]);
        }
    }
    return 0;
}
