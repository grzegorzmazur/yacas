
#include <stdio.h>
#include "lispevalhash.h"

void Emit(char* iname,char* fname,int flags)
{
  if (flags == (YacasEvaluator::Function | YacasEvaluator::Fixed))
    printf("10  # CompileFn(\"%s\") <-- \"%s\";\n",iname,fname);
}

int main(void)
{

#define CORE_KERNEL_FUNCTION(iname,fname,nrargs,flags) Emit(iname,#fname,flags);
#define CORE_KERNEL_FUNCTION_ALIAS(iname,fname,nrargs,flags) 

#include "corefunctions.h"
#undef CORE_KERNEL_FUNCTION
#undef CORE_KERNEL_FUNCTION_ALIAS
  return 0;
}