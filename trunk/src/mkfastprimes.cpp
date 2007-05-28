// this is really C++
#include <stdio.h>

const unsigned long primes_table_limit = 65537;

const unsigned long primes_table_size = (1+(primes_table_limit>>4));

unsigned char primes_table[primes_table_size];

int check_if_prime(const int p)
{
  int q;
  if (p & 1 == 0) return 0;
  for(q = 3; q*q <= p && (p % q != 0); q += 2);
  return (p % q == 0) ? 0 : 1;
}

void print_table()
{
  int i, p, q;
  for(i = 0; i < (int)primes_table_size; ++i)
    primes_table[i] = 0;
  // fill table
  for(p = 3; p <= (int)primes_table_limit; )
  {
    // mark p as prime in the bit field
    i = p >> 4;
    q = (p >> 1) & 7;
    primes_table[i] |= (1 << q);
    // find the next prime
    p += 2;
    for ( ;p <= (int)primes_table_limit && check_if_prime(p) == 0; p+=2);
  }
 
  printf(" const unsigned long primes_table_limit = %ld;\n const unsigned char primes_table[] = {\n", primes_table_limit);
  for(i=0; i<(int)primes_table_size; ++i)
    printf("0x%02x,\n", primes_table[i]);
  printf("};\n");
}

int main()
{
  print_table();
  return 0;
}
