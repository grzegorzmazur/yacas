
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char** argv)
{
  if (argc<3)
    exit(-1);
  FILE* fin = fopen(argv[1],"rb");
  if (!fin) exit(-1);
  FILE* fout = fopen(argv[2],"wb");
  if (!fout) exit(-1);
  
  char prev[8192];
  prev[0]=0;
  while (!feof(fin))
  {
    char buf[8192];
    fgets(buf,8192,fin);
    if (strcmp(buf,prev))
    {
      fprintf(fout,"%s",buf);
      strcpy(prev,buf);
    }
  }
  
  fclose(fin);
  fclose(fout);
  return 0;
}
