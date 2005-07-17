
#include <stdio.h>
#include "cyacas.h"

int verbose_debug = 0;	// avoid linkage error

void print_expr(void* object)
{
  if (object == NULL)
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
  void* result = yacas_execute(object);
  printf("Output>  ");
  print_expr(result);
  printf("\n");
  yacas_delete_object(result);
}

int main(int argc, char** argv)
{
  int i;
  yacas_init();
  
  void *input = 
    yacas_create_sublist(
      yacas_link_objects(
        yacas_create_atom("+"),
        yacas_link_objects(
          yacas_create_atom("1"),
          yacas_create_atom("1")
        )
      )
    );
  runexpr(input);

  yacas_delete_object(input);
  yacas_exit();
  return 0;
}
