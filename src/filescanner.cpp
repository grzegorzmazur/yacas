
#include <stdlib.h>
#include <string.h>
#include "filescanner.h"


CFileScanner::CFileScanner()
    : iCurNode()
#ifdef _GCC_BUILD_
  ,dp(NULL),entry(NULL),statbuf()
#endif
{
#ifdef WIN32
  handle = -1;
  first = 1;
#endif
}

CFileScanner::~CFileScanner()
{
#ifdef _GCC_BUILD_
    if (dp) closedir(dp);
#endif
}

CFileNode* CFileScanner::First(char* base,char* dir)
{
    strcpy(fulldir,base);
    strcat(fulldir,dir);
#ifdef WIN32
    if (strlen(dir)) strcat(fulldir,DIRSEP);
    strcat(fulldir,"*.*");
    first = 1;
#endif
    iCurNode.SetRoot(dir);
#ifdef _GCC_BUILD_
    if (dp) closedir(dp);
    dp = opendir(fulldir);
    if (!dp) return NULL;
#endif
    return Next();
}

CFileNode* CFileScanner::Next()
{
#ifdef WIN32
    if (first == 0 && handle == -1) return NULL;
REDO:
    if (first)
    {
        handle = _findfirst(fulldir, &info);
        if (handle == -1) return NULL;
        first = 0;
    }
    else
    {
        if (_findnext(handle, &info) != 0)
        {
            _findclose(handle);
            handle=-1;
            return NULL;
        }
    }
    if (!strcmp(info.name,".")) goto REDO;
    if (!strcmp(info.name,"..")) goto REDO;
    iCurNode.Set(info.attrib & _A_SUBDIR, info.name);
    return &iCurNode;

#endif


#ifdef _GCC_BUILD_
    if (!dp) return NULL;
REDO:
    entry = readdir(dp);
    if (!entry)
    {
        closedir(dp);
        dp = NULL;
        return NULL;
    }
    if (!strcmp(entry->d_name,".")) goto REDO;
    if (!strcmp(entry->d_name,"..")) goto REDO;
    stat(entry->d_name,&statbuf);
    char dum[500];
    strcpy(dum,fulldir);
    strcat(dum,"/");
    strcat(dum,entry->d_name);
    stat(dum,&statbuf);
    iCurNode.Set(S_ISDIR(statbuf.st_mode), entry->d_name);
    return &iCurNode;
#endif
    return NULL;
}

