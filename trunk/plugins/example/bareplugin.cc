
#include "stubs.h"

#include "bareplugin.h"

int add_integers(int a, int b)
{
    return a+b;
}
double add_doubles(double a, double b)
{
    return a+b;
}


/* Struct example */

Bla* CreateBla(int aa, int ab)
{
    Bla* bla = (Bla*)PlatAlloc(sizeof(Bla));
    bla->a = aa;
    bla->b = ab;
}
void BlaSetA(Bla* bla, int a)
{
    bla->a = a;
}
int BlaGetA(Bla* bla)
{
    return bla->a;
}
void Bla_free(Bla* bla)
{
    PlatFree((LispCharPtr)bla);
}
/*
 - StubApiCStructFree("base_Bla_free","Bla_free");
 - advanced: refcounting between classes.
*/


