#include <direct.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void writeFileToRamscript(char *filename, FILE* output);

int main(int argc, char *argv[])
{
	char dir[100];
	FILE *fout = stdout;
	long hFile;
	struct _finddata_t scriptfile;

	if(argc > 1)
		fout = fopen(argv[1], "w");

	strcpy(dir,"../scripts/");
	if(_chdir(dir)) {
		printf("Unable to locate the directory: %s\n", dir);
		exit(0);
	}
	  
    if((hFile = _findfirst( "*", &scriptfile)) == -1L)
		printf( "No files in current directory!\n" );

	// Something werid is going on here...
	if((scriptfile.attrib & _A_SUBDIR) != 16 ||
		scriptfile.size != 0)
		writeFileToRamscript(scriptfile.name, fout);

	while(_findnext(hFile, &scriptfile) == 0) {
		if((scriptfile.attrib & _A_SUBDIR) != 0 ||
			scriptfile.size == 0)
			continue;
		if(!memcmp(scriptfile.name,"Makefile",8))
            continue;

		writeFileToRamscript(scriptfile.name, fout);
	}

	_findclose(hFile);

	if(argc > 1)
		fclose(fout);

    return 0;
}


void writeFileToRamscript(char *filename, FILE *output)
{
	char c;
	FILE* fin = fopen(filename,"r");
	
	// File comment
	fprintf(output, "\n\n/* Script file: %s*/\n\n",filename);

	fprintf(output, "(*yacas)()().iRamDisk.SetAssociation(LispRamFile(\n");
	fprintf(output, "\"");
	while ((c=fgetc(fin)) != EOF) {
		switch (c) {
		case '\'':
		case '\\':
		case '\"':
			fprintf(output, "\\%c",c);
			break;
		case '\n':
			fprintf(output, " \"\n\"");
			break;
		default:
			fprintf(output, "%c",c);
		}
	}
	fprintf(output, "\"\n");
	fprintf(output, "),\n");
	fprintf(output, "(*yacas)()().HashTable().LookUp(\"%s\",LispTrue));\n"
				  , filename);
	
	fclose(fin);
}
