
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>

int main(int argc, char** argv)
{
    DIR *dp;
    struct dirent* entry;
    struct stat statbuf;
    char dir[500];
    strcpy(dir,"../scripts/"); //"/root/myprojects/yacas-latest/ramscripts");

    if (argc>1)
    {
        strcpy(dir,argv[1]); //"/root/myprojects/yacas-latest/ramscripts");
        
    }
    
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
    char filename[500];
    char c;
//    strcpy(filename, dir);
//    strcat(filename,entry->d_name);
    strcpy(filename,entry->d_name);
printf("// %s\n",filename);
    FILE* fin=fopen(filename,"r");

    printf("\"");
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
}
printf(
"),\n"
"      (*yacas)()().HashTable().LookUp(\"%s\",LispTrue));\n",entry->d_name);
//        printf("%s\t",entry->d_name);
    }
    return 0;
}
