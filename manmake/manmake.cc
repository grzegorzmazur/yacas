
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


void Emit(char* name, char* text)
{
    if (!strlen(name))
        return;
    if (!strcmp(name,"text") && !strlen(text))
        return;
        
    printf("ManP(%s,\"",name);

    while(*text)
    {
        switch (*text)
        {
        case '<':
            printf("&lt;");
            break;
        case '>':
            printf("&gt;");
            break;
        case '\\':
            printf("\\%c",text[1]);
            text++;
            break;
        case '\"':
            printf("\\\"");
            break;
        default:
            printf("%c",*text);
            break;
        }
        text++;
    }

    printf("\");\n");
}

void EmitLiteral(char* name, char* text)
{
    if (!strlen(name))
        return;
    if (!strcmp(name,"text") && !strlen(text))
        return;
    printf("ManP(%s,%s);\n",name,text);
}

int main(int argc, char* argv[])
{
    if (argc<2)
        return 0;

    int i;
    FILE* f=fopen(argv[1],"r");
    if (!f) return 0;


    fseek(f,0,SEEK_END);
    int fsize = ftell(f);
    char* buf = (char*)malloc(fsize);
    if (!buf)
        goto END;
    fseek(f,0,SEEK_SET);
    fread(buf,1,fsize,f);
    i=0;

    printf("ManP(enter);\n");

    while (i<fsize)
    {
        int start = i;
    RESKIP:
        while (i<fsize && buf[i] != '\\')i++;
        if (buf[i+1] == '\\')
        {
            i+=2;
            goto RESKIP;
        }
        buf[i] = ' ';
        {
            int j=i;
            while (j>start && isspace(buf[j])) j--;
            buf[j] = '\0';
        }
        Emit("text", &buf[start]);

        i++;
        start = i;
        while (i<fsize && buf[i] != '{' && buf[i] != '[')
        {
            if (buf[i] == '\\') i++;
            i++;
        }
        switch (buf[i])
        {
        case '{':
            {
                buf[i] = '\0';
                i++;
                int nameend = i;
                while (i<fsize && buf[i] != '}')
                {
                    if (buf[i] == '\\') i++;
                    i++;
                }

                buf[i] = '\0';
                Emit(&buf[start],&buf[nameend]);
                i++;
            }
            break;
        case '[':
            {
                buf[i] = '\0';
                i++;
                int nameend = i;
                while (i<fsize && buf[i] != ']')
                {
                    if (buf[i] == '\\') i++;
                    i++;
                }
                buf[i] = '\0';
                EmitLiteral(&buf[start],&buf[nameend]);
            }
            break;
        }
    }

    printf("ManP(leave);\n");
    
    free(buf);
END:
    fclose(f);
    return 0;
}

