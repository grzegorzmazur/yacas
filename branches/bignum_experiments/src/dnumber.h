#ifndef DNUMBER_H
#define DNUMBER_H

#include <cstddef>
#include <ostream>
#include <vector>
#include <string>

/** Decimal arbitrary precision number.
 */
class DNumber {
public:
    /** Construct a number representing zero */
    DNumber();
    /** Construct a number representing given string */
    explicit DNumber(const char*);

    /** Construct a number from given integer */
    explicit DNumber(long int);

    DNumber& operator = (const DNumber&);

    bool operator == (const DNumber&) const;
    bool operator != (const DNumber&) const;

    bool operator < (const DNumber&) const;
    bool operator > (const DNumber&) const;

    bool operator <= (const DNumber&) const;
    bool operator >= (const DNumber&) const;

    /** Add given number */
    DNumber& operator += (const DNumber&);
    /** Subtract given number */
    DNumber& operator -= (const DNumber&);
    /** Multiply by given number */
    DNumber& operator *= (const DNumber&);
    /** Divide by given number */
    DNumber& operator /= (const DNumber&);

    /** Return true if the number is equal to zero */
    bool is_zero() const;
    bool is_positive() const;
    /** Return true if the number has no fractional part */
    bool is_integer() const;
    /** Return true if the number is even */
    bool is_even() const;

    /** Return a string representing the number */
    std::string to_string() const;

    /** Negate the number */
    void neg();
    /** Take absolute value of the number */
    void abs();

    /** Round the number from zero*/
    void round(std::size_t);

    /** Display the internal representation of the number */
    void dump(std::ostream&);

    /** The precision with which division is performed */
    static std::size_t div_prec;

private:
    /** Add given number */
    void add(const DNumber&);
    /** Subtract given number */
    void sub(const DNumber&);

    /** Multiply by given number */
    void mul(const DNumber&);
    /** Divide by given number */
    void div(const DNumber&, std::size_t);

    void zero();
    void normalize();
    void scale(std::size_t);
    void expand(std::size_t);

    typedef unsigned char Byte;
    typedef signed char SignedByte;

    bool _zero;
    bool _minus;
    std::size_t _no_digits;
    int _exponent;
    std::vector<Byte> _rep;
};

inline
bool DNumber::operator != (const DNumber& rhs) const
{
    return !(*this == rhs);
}

inline
bool DNumber::operator > (const DNumber& rhs) const
{
    return !(*this <= rhs);
}

inline
bool DNumber::operator <= (const DNumber& rhs) const
{
    return *this < rhs || *this == rhs;
}

inline
bool DNumber::operator >= (const DNumber& rhs) const
{
    return !(*this < rhs);
}

inline
DNumber& DNumber::operator += (const DNumber& x)
{
    this->add(x);
    return *this;
}

inline
DNumber& DNumber::operator -= (const DNumber& x)
{
    this->sub(x);
    return *this;
}

inline
DNumber& DNumber::operator *= (const DNumber& x)
{
    this->mul(x);
    return *this;
}

inline
DNumber& DNumber::operator /= (const DNumber& x)
{
    this->div(x, div_prec);
    return *this;
}

inline
bool DNumber::is_zero() const
{
    return _zero;
}

inline
bool DNumber::is_positive() const
{
    return !_zero && !_minus;
}

inline
void DNumber::neg()
{
    if (!_zero) 
        _minus = !_minus;
}

inline
void DNumber::abs()
{
    _minus = false;
}

inline
DNumber operator + (const DNumber& x, const DNumber& y)
{
    DNumber z(x);
    z += y;
    return z;
}

inline
DNumber operator - (const DNumber& x, const DNumber& y)
{
    DNumber z(x);
    z -= y;
    return z;
}

inline
DNumber operator * (const DNumber& x, const DNumber& y)
{
    DNumber z(x);
    z *= y;
    return z;
}

inline
DNumber operator / (const DNumber& x, const DNumber& y)
{
    DNumber z(x);
    z /= y;
    return z;
}

#endif
