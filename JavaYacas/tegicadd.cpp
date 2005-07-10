
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int main(int argc,char** argv)
{
  FILE*fin=stdin;
  FILE*fout=stdout;
  char buf[8192];
  while (!feof(fin))
  {
    fgets(buf,8192,fin);
    fprintf(fout,"~");
    char* ptr;
    ptr = &buf[0];
    while (*ptr)
    {
      if (*ptr == ':')
        *ptr = '~';
      ptr++;
    }

    ptr = &buf[1];
    while (*ptr != '~')
    {
      if (isdigit(*ptr))
      {
        fprintf(fout,"%c",*ptr);
      }
      else if (isalpha(*ptr))
      {
        char c = toupper(*ptr);
             if (c>='A' && c<= 'C') fprintf(fout,"2");
        else if (c>='D' && c<= 'F') fprintf(fout,"3");
        else if (c>='G' && c<= 'I') fprintf(fout,"4");
        else if (c>='J' && c<= 'L') fprintf(fout,"5");
        else if (c>='M' && c<= 'O') fprintf(fout,"6");
        else if (c>='P' && c<= 'S') fprintf(fout,"7");
        else if (c>='T' && c<= 'V') fprintf(fout,"8");
        else if (c>='W' && c<= 'Z') fprintf(fout,"9");
      }
      else
      {
        fprintf(fout,"1");
      }

      ptr++;
    }
    fprintf(fout," %s",buf);
  }
  
  return 0;
}

