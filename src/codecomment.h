
#ifndef __codecomment_h__
#define __codecomment_h__

#include <stdlib.h>
#include "grower.h"

class CodeComment : public YacasBase
{
public:
	typedef CArrayGrower<char *, ArrOpsCustomPtr<char> > Type;
    
	static Type * pCodeComments;	// scalar-pointer avoids multiple construction problem with statics
	CodeComment(const char * aString, int aPriority = 0)
	{
		if (!pCodeComments)
			pCodeComments = NEW Type;
		pCodeComments->Append(strdup(aString));
	}
  static void Reset()
  {
    if (pCodeComments)
    {
      delete pCodeComments;
      pCodeComments = NULL;
    }
  }
  
/*
	CodeComment(const char * aString, int aPriority = 0)
	{
		if (!pCodeComments)
		{
			printf("CodeComment(...){ pCodeComments = NEW Type; }\n");
			pCodeComments = NEW Type;
		}
		int size = pCodeComments->Size();
		if (!size)
		{
			printf("CodeComment(\"%s\")\n", aString);
			printf("&CodeComment::pCodeComments = 0x%p\n", &pCodeComments);
			printf("CodeComment::pCodeComments = 0x%p\n", pCodeComments);
			printf("CodeComment::pCodeComments->Size() = %d\n", size);
		}
		pCodeComments->Append(strdup(aString));
		if (!size)
		{
			size = pCodeComments->Size();
			char ** ptr = &(*pCodeComments)[size-1];
			printf("&(*CodeComment::pCodeComments)[%d] = 0x%p -> \"%s\"\n",
				size-1, ptr, *ptr);
		}
	}
	*/
};

#endif

