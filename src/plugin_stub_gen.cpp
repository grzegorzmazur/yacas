

#include <stdio.h>
#include <stdlib.h>
int main(int argc, char** argv)
{
  if (argc<3) exit(-1);
  FILE*f=fopen(argv[1],"wb");
  if (!f) exit(-1);
  fprintf(f,"#include \"plugins_available.h\"\n#ifdef EXE_DLL_PLUGINS\n#include \"%s\"\n#endif // EXE_DLL_PLUGINS\n",argv[2]);
  fclose(f);
  return 0;
}







