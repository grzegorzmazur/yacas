
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


#define FOREGROUND 0x000000
#define BACKGROUND 0xffffff
//#define FOREGROUND 0xffffff
//#define BACKGROUND 0x000000

FILE* indexfp;


enum ColorModes
{
    EModeDefault,   //ForeGroundBlack
    EModeComment,   //ForeGroundGreen
    EModeString,    //ForeGroundYellow
    EModeBrackets,  //ForeGroundBlue
    EModeOperator1, //ForeGroundBlue
    EModeOperator2, //ForeGroundMagenta
    EModeNumber,    //ForeGroundRed
};

int quoted=0,commented=0;
void ResetColorPrint()
{
    quoted=0;
    commented=0;
}

void ColorMode(FILE* fp,int type)
{
    int color=FOREGROUND;
    switch (type)
    {
    case EModeDefault:   //ForeGroundBlack
        color = FOREGROUND; break;
    case EModeComment:   //ForeGroundGreen
        color = 0x00ff00; break;
    case EModeString:    //ForeGroundYellow
        color = 0xa2a200; break;
    case EModeBrackets:  //ForeGroundBlue
        color = 0x0000ff; break;
    case EModeOperator1: //ForeGroundBlue
        color = 0x0000ff; break;
    case EModeOperator2: //ForeGroundMagenta
        color = 0xff00ff; break;
    case EModeNumber:    //ForeGroundRed
        color = 0xff0000; break;
    }
    fprintf(fp,"</font><font color=%.6x>",color);
}

void ColorPrint(FILE* out,char* buf)
{
    int i,nr=strlen(buf);
    for (i=0;i<nr;i++)
    {
        if (commented || quoted)
        {
            
            fprintf(out,"%c",buf[i]);
            if (buf[i+1] == '/' && buf[i] == '*')
            {
                fprintf(out,"%c",buf[i+1]);
                i++;
                commented=0;
            }
            if (buf[i] == '\"')
            {
                quoted=0;
            }
        }
        else
        {
            if (buf[i] == '/' && buf[i+1] == '*')
            {
                ColorMode(out,EModeComment);
                commented=1;
            }
            else if (buf[i] == '\"')
            {
                ColorMode(out,EModeString);
                quoted=1;
            }
            else if (strchr("{}()[]",buf[i])!=NULL)
            {
                ColorMode(out,EModeBrackets);
            }
            else if (strchr("+-*/=:!^<>",buf[i])!=NULL)
            {
                ColorMode(out,EModeOperator1);
            }
            else if (strchr("_,;#",buf[i])!=NULL)
            {
                ColorMode(out,EModeOperator2);
            }
            else if (strchr("0123456789.",buf[i])!=NULL)
            {
                ColorMode(out,EModeNumber);
            }
            else
            {
                ColorMode(out,EModeDefault);
            }
            fprintf(out,"%c",buf[i]);
        }
    }
}


int nrops;
#define MAXOPS 100
char *ops[MAXOPS];

char root[128];

void PutFile(FILE* out, FILE* in)
{
    char buffer[1000];

    fgets(buffer,1000,in);

    ResetColorPrint();
    while (!feof(in))
    {
        int i;
        for (i=0;i<nrops;i++)
        {
            if (ops[i])
            {
                if (strstr(buffer, ops[i]))
                {
                    fprintf(out,"<A NAME=\"%s\"></A>",ops[i]);
                    free(ops[i]);
                    ops[i]=NULL;
                }
            }
        }

        ColorPrint(out,buffer);

        fgets(buffer,1000,in);
    }
}


void Htmlize(char* filename)
{
    FILE *fp,*deffp,*htmlfp;
    char infile[256];
    char deffile[256];
    char htmlfile[256];
    strcpy(infile,root);
    strcat(infile,filename);
    
    fp=fopen(infile,"r");
    if (fp == NULL)
    {
        printf("Warning: could not find file %s\n",infile);
        return;
    }

    strcpy(htmlfile,filename);
    strcat(htmlfile,".html");
    htmlfp=fopen(htmlfile,"w");
    if (htmlfp == NULL)
    {
        fclose(fp);
        return;
    }

    fprintf(indexfp,"<LI>\n<A HREF=\"%s\"  TARGET=\"Chapters\">%s</A></LI>\n",htmlfile,filename);

    strcpy(deffile,"../scripts/");
    strcat(deffile,filename);
    strcat(deffile,".def");
    deffp=fopen(deffile,"r");

    fprintf(htmlfp,"<html>\n<head>\n<title>%s</title>\n</head>\n",filename);
    fprintf(htmlfp,"<body BGCOLOR=\"ffffff\" LINK=\"0000ff\" VLINK=\"0000ff\">");

    nrops=0;
    if (deffp)
    {
        char op[100];
        fscanf(deffp,"%s",op);
        while(!feof(deffp))
        {
            if (strcmp(op,"}"))
            {
                assert(nrops<MAXOPS);
                fprintf(htmlfp,"<a href=\"%s#%s\">%s</a> ",htmlfile,op,op);
                ops[nrops++] = strdup(op);
            }
            fscanf(deffp,"%s",op);
        }
    }
    
    fprintf(htmlfp,"<h1>%s</h1>\n",filename);

    fprintf(htmlfp,"<table>\n<TR><TD WIDTH=100%% bgcolor=%.6x>\n<PRE>",BACKGROUND);
    fprintf(htmlfp,"<font color=%.6x>",FOREGROUND);
    
    PutFile(htmlfp,fp);

    fprintf(htmlfp,"</font></PRE>\n</TABLE>\n</body>\n</html>\n");

    if (deffp)
        fclose(deffp);
    fclose(htmlfp);
    fclose(fp);

    {
        int i;
        for (i=0;i<nrops;i++)
        {
            free(ops[i]);
        }
        nrops=0;
    }
}


int main(void)
{
    indexfp = fopen("scriptsindex.html","w");
    if (indexfp==NULL)
    {
        printf("Error : could not create index\n");
        return 0;
    }
    fprintf(indexfp,"<HTML>\n<BODY BGCOLOR=\"ffffff\">\n<UL>\n");

    strcpy(root,"../scripts/examples/");
    fprintf(indexfp,"</UL><H1>Examples</H1><UL>");
    Htmlize("plot2d");
    Htmlize("queens");

    strcpy(root,"../scripts/");
    fprintf(indexfp,"</UL><H1>Source</H1><UL>");
    Htmlize("array");
    Htmlize("assoc");
    Htmlize("complex");
    Htmlize("constants");
    Htmlize("controlflow");
    Htmlize("deffunc");
    Htmlize("deriv");
    Htmlize("edit");
    Htmlize("example");
    Htmlize("factors");
    Htmlize("fakedb");
    Htmlize("formula");
    Htmlize("functional");
    Htmlize("html");
    Htmlize("integrate");
    Htmlize("linalg");
    Htmlize("lists");
    Htmlize("newly");
    Htmlize("numbers");
    Htmlize("padic");
    Htmlize("patterns");
    Htmlize("predicates");
    Htmlize("random");
    Htmlize("simplify");
    Htmlize("standard");
    Htmlize("stdfuncs");
    Htmlize("stdopers");
    Htmlize("solve");
    Htmlize("stats");
    Htmlize("stubs");
    Htmlize("substitute");
    Htmlize("sums");
    Htmlize("tensor");
    Htmlize("testers");
    Htmlize("trigsimp");
    Htmlize("univar");
    Htmlize("yacasinit");


    fprintf(indexfp,"</UL>\n</BODY>\n</HTML>\n");
    fclose(indexfp);
    {
        FILE*fp;
        fp=fopen("scriptsmain.html","w");

        fprintf(fp,"<HTML>\n");
        fprintf(fp,"<FRAMESET BORDER=\"0\" COLS=\"150,*\">\n");
        fprintf(fp,"  <FRAME SRC=\"scriptsindex.html\">\n");
        fprintf(fp,"  </FRAME>\n");
        fprintf(fp,"\n");
        fprintf(fp,"  <FRAME SRC=\"scriptsintro.html\" NAME=\"Chapters\">\n");
        fprintf(fp,"  </FRAME>\n");
        fprintf(fp,"</FRAMESET>\n");
        fprintf(fp,"</HTML>\n");
        fclose(fp);

        fp=fopen("scriptsintro.html","w");
        fprintf(indexfp,"<HTML>\n<BODY BGCOLOR=\"ffffff\">\n");
        fprintf(indexfp,"This page allows you to browse the standard library ");
        fprintf(indexfp,"scripts that come with Yacas. The left pane shows ");
        fprintf(indexfp,"a list of the files in the distribution. At the ");
        fprintf(indexfp,"top of each file are links to the places the ");
        fprintf(indexfp,"functions are mentioned for the first time.");
        fprintf(indexfp,"</BODY>\n</HTML>\n");
        fclose(fp);
    }
    
    
    
    return 0;
}

