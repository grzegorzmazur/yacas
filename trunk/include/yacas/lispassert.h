#ifndef YACAS_LISPASSERT_H
#define YACAS_LISPASSERT_H


#include "choices.h"

#ifdef USE_ASSERT
  #include <assert.h>
  #define LISPASSERT(x)  assert(x)
#else
  #define LISPASSERT(x)
#endif

#endif

