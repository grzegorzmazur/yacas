#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
  This executable has one output: it updates the file plugins_available.h. This header file contains
  one define, EXE_DLL_PLUGINS. This symbol is defined if and only if: DISABLE_DYNAMIC is defined,
  and the plugin files like libmath.cpp are available. For if DISABLE_DYNAMIC is not defined, we
  want to load plugins as dlls. But if it is defined, we can not compile the plugins in yet because
  the files are not yet available.
*/

int main(int argc, char** argv)
{
  char* define = "//#define";
#ifdef DISABLE_DYNAMIC 
  {
//printf("1...\n");
    FILE* f=fopen("libmath.cpp","rb");
	if (f)
	{
//printf("2...\n");
	  // DISABLE_DYNAMIC is defined, but the plugin files are available, so compile them in statically
      define = "#define";
	  fclose(f);
	}
//printf("3...\n");
  }
#endif // DISABLE_DYNAMIC

  char new_buf[1024];
  sprintf(new_buf,"#ifndef __plugins_available_h__\n#define __plugins_available_h__\n%s EXE_DLL_PLUGINS\n#endif /* __plugins_available_h__ */\n",define);

  // To stop recursive recompiles, only write the file if it did not yet exist, or if it changed.
  int do_save = 1;	// equivalent to "changed"
  char old_buf[1024];
  old_buf[0] = '\0';
  {
    FILE*f = fopen("plugins_available.h","rb");
	if (f)
	{
	  fseek(f,0,SEEK_END);
	  int size = ftell(f);
	  fseek(f,0,SEEK_SET);
	  fread(old_buf,1,size,f);
	  old_buf[size] = '\0';
	  fclose(f);
	  if (!strcmp(old_buf,new_buf))
	  {
	    do_save = 0;
		printf("plugins_available.h already up to date, not resaving\n");
	  }
	}
	else
	{
	  fprintf(stderr,"plugins_available.h did not yet exist, creating it now\n");
	}
  }
  if (do_save)
  {
    printf("Saving plugins_available.h\n");
    FILE*f = fopen("plugins_available.h","wb");
	if (f)
	{
      fprintf(f,"%s",new_buf);
	  fclose(f);
	}
	else
	{
	  printf("warning: could not create file plugins_available.h\n");
	}
  }
  return do_save;
}
