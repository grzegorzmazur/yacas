
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>

int main(void)
{
    DIR *dp;
    struct dirent* entry;
    struct stat statbuf;
    char dir[100];
    strcpy(dir,"../scripts/"); //"/root/myprojects/yacas-latest/ramscripts");
    
    if ((dp = opendir(dir)) == NULL)
        exit(0);
    chdir(dir);
    while ((entry = readdir(dp)) != NULL)
    {
        stat(entry->d_name,&statbuf);
        if (S_ISDIR(statbuf.st_mode))
            continue;
        if (!memcmp(entry->d_name,"Makefile",8))
            continue;

printf(
"      (*yacas)()().iRamDisk.SetAssociation(\n"
"        LispRamFile(\n");

{
    char filename[100];
    char c;
    strcpy(filename, dir);
    strcat(filename,entry->d_name);
    FILE* fin=fopen(filename,"r");

    printf("\"");
    while ((c=fgetc(fin)) != EOF)
    {
        switch (c)
        {
        case '\'':
        case '\\':
        case '\"':
            printf("\\%c",c);
            break;
        case '\n':
            printf(" \"\n\"");
            break;
        default:
            printf("%c",c);
        }
    }
    printf("\"\n");
    
    fclose(fin);
}
printf(
"),\n"
"      (*yacas)()().HashTable().LookUp(\"%s\",LispTrue));\n",entry->d_name);
//        printf("%s\t",entry->d_name);
    }
    return 0;
}
