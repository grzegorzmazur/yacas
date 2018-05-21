/* Math using the standard library, if the precision is less than 13 */
#include "yacas/platmath.h"
#include "yacas/errors.h"
#include "yacas/lispatom.h"
#include "yacas/lispenvironment.h"
#include "yacas/lispobject.h"
#include "yacas/numbers.h"

#include <bitset>
#include <cmath>
#include <sstream>

double GetDouble(LispObject* aInteger)
{
    BigNumber* number = aInteger->Number(0);
    if (!number) {
        std::ostringstream buf;
        buf << "Argument is not a number: " << aInteger->String();
        throw LispErrGeneric(buf.str());
    }
    return number->Double();
}

LispObject* Double(LispEnvironment& aEnvironment, double aValue)
{
    std::ostringstream buf;
    buf << aValue;
    return LispAtom::New(aEnvironment, buf.str());
}

LispObject*
PlatArcSin(LispEnvironment& aEnvironment, LispObject* int1, int aPrecision)
{
    return Double(aEnvironment, std::asin(GetDouble(int1)));
}

LispObject*
PlatLn(LispEnvironment& aEnvironment, LispObject* int1, int aPrecision)
{
    return Double(aEnvironment, std::log(GetDouble(int1)));
}

LispObject* PlatPower(LispEnvironment& aEnvironment,
                      LispObject* int1,
                      LispObject* int2,
                      int aPrecision)
{
    return Double(aEnvironment, std::pow(GetDouble(int1), GetDouble(int2)));
}

LispObject* PlatDiv(LispEnvironment& aEnvironment,
                    LispObject* int1,
                    LispObject* int2,
                    int aPrecision)
{
    return Double(aEnvironment,
                  ((long)GetDouble(int1)) / ((long)GetDouble(int2)));
}

namespace {
    static const std::size_t MAX_SMALL_PRIME = 65537;

    static std::bitset<MAX_SMALL_PRIME / 2 + 1> _primes_table;

    static class InitPrimesTable {
    public:
        InitPrimesTable();
    } _init_primes_table;

    InitPrimesTable::InitPrimesTable()
    {
        for (std::size_t i = 3; i < MAX_SMALL_PRIME; i += 2) {
            if (_primes_table.test(i / 2))
                continue;
            for (std::size_t j = 3; j < MAX_SMALL_PRIME / i; j += 2)
                _primes_table.set((i * j) / 2);
        }
    }
}

unsigned primes_table_check(unsigned long p)
{
    if (p == 0)
        return MAX_SMALL_PRIME;

    if (p == 2)
        return 1;

    if (p < 2 || p > MAX_SMALL_PRIME || (p & 1) == 0)
        return 0;

    return !_primes_table.test(p / 2);
}

LispObject*
PlatIsPrime(LispEnvironment& aEnvironment, LispObject* int1, int aPrecision)
{
    return Double(aEnvironment,
                  primes_table_check((unsigned long)(GetDouble(int1))));
}
