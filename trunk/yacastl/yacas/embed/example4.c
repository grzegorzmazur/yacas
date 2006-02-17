
#include <stdio.h>
#include "cyacas.h"

int verbose_debug = 0;	// avoid linkage error

void print_attrs(void* object)
{
	char* attrs[] = {
		!object                                     ? "null" : NULL,
		(object && yacas_object_is_string(object))  ? "string" : NULL,
		(object && yacas_object_is_sublist(object)) ? "sublist" : NULL,
		(object && yacas_object_is_atom(object))    ? "atom" : NULL,
		(object && yacas_object_is_number(object))  ? "number" : NULL,
		(object && yacas_object_is_integer(object)) ? "integer" : NULL,
		NULL };
	int count = 0, ii;
	printf("attrs: [");
	for (ii = 0; ii < sizeof(attrs)/sizeof(attrs[0]); ii++)
	{
		if (!attrs[ii]) continue;
		if (count) printf(", ");
		printf("%s", attrs[ii]);
		count++;
	}
	printf("]\n");
}

void print_expr(void* object)
{
  if (!object)
    return;

  if (yacas_object_is_sublist(object))
  {
    printf("(");
    print_expr(yacas_get_sublist(object));
    printf(")");
  }
  else
  {
    printf("%s ",yacas_get_atom(object));
    print_expr(yacas_get_next(object));
  }
}

void runexpr(void* object)
{
  printf("Input>  ");
     print_expr(object);
     printf("\n");
     print_attrs(object);
  void* result = yacas_execute(object);
     printf("Output>  ");
     print_expr(result);
     printf("\n");
     print_attrs(result);
  yacas_delete_object(result);
}

int main(int argc, char** argv)
{
  void* stuff_to_sum = NULL;
  void* helper = NULL;

  yacas_init();

// yacas_link_objects(pH,pT) == pH
#define summand( s ) \
  { \
    void* temp = s; /* so s is expanded nly once in this macro */ \ 
    if (!stuff_to_sum) { helper = stuff_to_sum = temp; } \
    else { yacas_link_objects(helper, temp); helper = temp; }; \
  }

  summand( yacas_create_atom("31") );
  summand( yacas_create_atom("32") );
//
  summand( yacas_create_atom("33") );
  summand( yacas_create_atom("36") );
  summand( yacas_create_atom("34") );
  summand( yacas_create_atom("11") );
//
  void *input = 
     yacas_create_sublist(
        yacas_link_objects(
           yacas_create_atom("+"),
#if 1
           stuff_to_sum
#else
		   yacas_link_objects(
		      yacas_create_atom("31"), 
		      yacas_create_atom("32")
		   )
#endif
        )
     );
  runexpr(input);

  yacas_delete_object(input);
  yacas_exit();
  return 0;
}
