
int add_integers(int a, int b);
double add_doubles(double a, double b);


typedef struct Bla
{
    int a;
    int b;
} Bla;

Bla* CreateBla(int aa, int ab);
void BlaSetA(Bla* bla, int a);
int BlaGetA(Bla* bla);
void Bla_free(void* bla);

