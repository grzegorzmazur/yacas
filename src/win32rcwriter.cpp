
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void)
{
  FILE* fin=fopen("scripts.dat","rb");
  if (!fin)
  {
    printf("Could not open scripts.dat!\n");
    exit(0);
  }
  FILE* fout=fopen("scripts.rc","w");
  if (!fout)
  {
    printf("Could not open scripts.rc!\n");
    fclose(fin);
    exit(0);
  }



  fprintf(fout,"RC_DATA1 RCDATA \n");
  fprintf(fout,"BEGIN\n");
  int i=0;
  while (!feof(fin))
  {
    int c1,c2;
    c1 = fgetc(fin);
    c2 = fgetc(fin);
    if (i) fprintf(fout,", \n");
    fprintf(fout,"    0x%.4x",(c2<<8)+c1);
    i++;
  }
  fprintf(fout,"\n");
  fprintf(fout,"END\n");


  fclose(fout);
  fclose(fin);
  return 0;
}


