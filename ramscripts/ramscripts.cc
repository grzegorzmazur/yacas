
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>

char maindir[500];


void ProcessFile(char* filename, char* dir)
{
printf(
"      (*yacas)()().iRamDisk.SetAssociation(\n"
"        LispRamFile(\n");

    printf("// %s%s\n",dir,filename);
    FILE* fin=fopen(filename,"r");

    if (!fin)
    {
        fprintf(stderr,"Could not open %s%s!\n",dir,filename);
        return;
    }
    printf("\"");
    char c;
    while ((c=fgetc(fin)) != EOF)
    {
        switch (c)
        {
        case '/':
            {
                char c2 = fgetc(fin);
                if (c2 == '/')
                {
                    while (c!='\n') c = fgetc(fin);
                    break;
                }
                else if (c2 == '*')
                {
                    c = fgetc(fin);
                    c2 = fgetc(fin);
                    for (;;)
                    {
                        if (c == '*' && c2 == '/')
                            break;
                        c = c2;
                        c2 = fgetc(fin);
                    }
                    break;
                }
                else
                {
                    ungetc(c2,fin);
                    goto NORMAL;
                }
            }
        case '\'':
        case '\\':
        case '\"':
            printf("\\%c",c);
            break;
        case '\n':
            printf(" \"\n\"");
            break;
        default:
        NORMAL:
            printf("%c",c);
        }
    }
    printf("\"\n");
    
    fclose(fin);
    printf(
           "),\n"
           "      (*yacas)()().HashTable().LookUp(\"%s%s\",LispTrue));\n",dir,filename);
}


void ProcessDirectory(char* dir)
{
    DIR *dp;
    struct dirent* entry;
    struct stat statbuf;

    char fulldir[500];
    strcpy(fulldir,maindir);
    strcat(fulldir,dir);
    fulldir[strlen(fulldir)-1] = '\0';
    
    if ((dp = opendir(fulldir)) == NULL)
    {
        fprintf(stderr,"Error opening directory!\n");
        exit(0);
    }
    chdir(fulldir);
    while ((entry = readdir(dp)) != NULL)
    {
        stat(entry->d_name,&statbuf);
        if (!strcmp(entry->d_name,"CVS"))
        {
            fprintf(stderr,"Skipping [%s]...\n",entry->d_name);
            continue;
        }
        if (!strcmp(entry->d_name,"."))
        {
            fprintf(stderr,"Skipping [%s]...\n",entry->d_name);
            continue;
        }
        if (!strcmp(entry->d_name,".."))
        {
            fprintf(stderr,"Skipping [%s]...\n",entry->d_name);
            continue;
        }
        if (!memcmp(entry->d_name,"Makefile",8))
        {
            fprintf(stderr,"Skipping [%s]...\n",entry->d_name);
            continue;
        }
        if (S_ISDIR(statbuf.st_mode))
        {
            char dirname[500];
            strcpy(dirname,dir);
            strcat(dirname,entry->d_name);
            strcat(dirname,"/");
            fprintf(stderr,"Directory %s\n",dirname);

            ProcessDirectory(dirname);
            chdir(fulldir);
            continue;
        }

        fprintf(stderr,"File %s%s\n",dir,entry->d_name);
        ProcessFile(entry->d_name, dir);
    }

}

int main(int argc, char** argv)
{
    strcpy(maindir,"../scripts/"); //"/root/myprojects/yacas-latest/ramscripts");

    if (argc>1)
    {
        strcpy(maindir,argv[1]); //"/root/myprojects/yacas-latest/ramscripts");
    }

    ProcessDirectory("");

    return 0;
}
