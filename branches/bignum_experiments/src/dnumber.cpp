#include "dnumber.h"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <sstream>

#include <iostream>
#include <iomanip>


namespace {
    static const char* skipws(const char* s)
    {
        while (*s && std::isspace(*s))
            s += 1;

        return s;
    }
}

std::size_t DNumber::div_prec = 16;

DNumber::DNumber()
{
    zero();
}

DNumber::DNumber(const char* s):
    _zero(false),
    _minus(false),
    _exponent(0)
{
    s = skipws(s);

    if (*s == '+')
        s += 1;

    if (*s == '-') {
        _minus = true;
        s += 1;
    }

    s = skipws(s);

    const char* e = std::strchr(s, 'e');
    if (!e)
        e = std::strchr(s, 'E');

    if (e)
        _exponent = std::atol(e + 1);

    if (!e)
        e = s + std::strlen(s);

    _no_digits = e - s;

    const char* d = std::strchr(s, '.');

    if (d) {
        _no_digits -= 1;
        _exponent -= e - d - 1;
    }

    _exponent += _no_digits;

    const std::size_t nb =
        _no_digits % 2 ? (_no_digits + 1) / 2 : _no_digits / 2;

    _rep.resize(nb);

    const char* p = s;

    for (std::size_t b = 0; b + 1 < nb; ++b) {
        if (*p == '.')
            p += 1;
        _rep[b] = (*p++ - '0') * 10;
        if (*p == '.')
            p += 1;
        _rep[b] += *p++ - '0';
    }

    if (_no_digits % 2) {
        if (*p == '.')
            p += 1;
        _rep[nb - 1] = (*p++ - '0') * 10;
    } else {
        if (*p == '.')
            p += 1;
        _rep[nb - 1] = (*p++ - '0') * 10;
        if (*p == '.')
            p += 1;
        _rep[nb - 1] += *p++ - '0';
    }

    normalize();
}

DNumber::DNumber(long int n)
{
    if (n == 0) {
        zero();
        return;
    }

    std::stringstream s;
    s << n;
    DNumber d(s.str().c_str());

    *this = d;
}

DNumber& DNumber::operator = (const DNumber& d)
{
    _zero = d._zero;
    _minus = d._minus;

    _no_digits = d._no_digits;
    _exponent = d._exponent;
    _rep = d._rep;

    return *this;
}

bool DNumber::operator == (const DNumber& rhs) const
{
    if (_zero || rhs._zero)
        return _zero && rhs._zero;

    if ((_minus && !rhs._minus) || (!_minus && rhs._minus))
        return false;

    if (_no_digits != rhs._no_digits)
        return false;

    if (_exponent != rhs._exponent)
        return false;

    return _rep == rhs._rep;
}

bool DNumber::operator < (const DNumber& rhs) const
{
    if (_zero) {
        if (rhs._zero || rhs._minus)
            return false;

        return true;
    }

    if (rhs._zero)
        return _minus;

    if (_minus && !rhs._minus)
        return true;

    if (!_minus && rhs._minus)
        return false;

    if (_exponent > rhs._exponent)
        return _minus;

    if (_exponent < rhs._exponent)
        return !_minus;

    const bool t =
        std::lexicographical_compare(
            _rep.begin(), _rep.end(),
            rhs._rep.begin(), rhs._rep.end());

    if (_minus)
        return !t;

    return t;
}

bool DNumber::is_integer() const
{
    return _zero || (_exponent >= 0 && static_cast<std::size_t>(_exponent) >= _no_digits);
}

bool DNumber::is_even() const
{
    assert(is_integer());

    if (_zero)
        return true;

    if (_exponent >= 0 && static_cast<std::size_t>(_exponent) > _no_digits)
        return true;

    Byte b = _rep[_rep.size() - 1];
    if (_no_digits & 1)
        b = b / 10;

    return !(b & 1);
}

std::string DNumber::to_string() const
{
    if (_zero)
        return "0";

    std::string s;

    if (_minus)
        s.push_back('-');

    if (is_integer()) {
        const Byte* p = &_rep[0];

        const std::size_t nb =
            _no_digits % 2 ? (_no_digits + 1) / 2 : _no_digits / 2;

        for (std::size_t b = 1; b < nb; ++b) {
            s.push_back(*p / 10 + '0');
            s.push_back(*p % 10 + '0');

            p += 1;
        }

        if (_no_digits % 2) {
            s.push_back(*p / 10 + '0');
        } else {
            s.push_back(*p / 10 + '0');
            s.push_back(*p % 10 + '0');
        }

        for (std::size_t i = _no_digits; i != static_cast<std::size_t>(_exponent); ++i)
            s.push_back('0');
    } else {
        if (_exponent <= 0) {
            s += "0.";
            const Byte* p = &_rep[0];

            const std::size_t nb =
                _no_digits % 2 ? (_no_digits + 1) / 2 : _no_digits / 2;

            for (std::size_t b = 1; b < nb; ++b) {
                s.push_back(*p / 10 + '0');
                s.push_back(*p % 10 + '0');

                p += 1;
            }

            if (_no_digits % 2) {
                s.push_back(*p / 10 + '0');
            } else {
                s.push_back(*p / 10 + '0');
                s.push_back(*p % 10 + '0');
            }

            p += 1;

            if (_exponent < 0) {
                std::ostringstream os;
                os << _exponent;

                s += "e" + os.str();
            }

        } else {
            std::size_t d = 0;

            const Byte* p = &_rep[0];

            const std::size_t nb =
                _no_digits % 2 ? (_no_digits + 1) / 2 : _no_digits / 2;

            for (std::size_t b = 0; b + 1 < nb; ++b) {
                s.push_back(*p / 10 + '0');
                if (++d == std::size_t(_exponent))
                    s += ".";
                s.push_back(*p % 10 + '0');
                if (++d == std::size_t(_exponent))
                    s += ".";

                p += 1;
            }

            if (_no_digits % 2) {
                s.push_back(*p / 10 + '0');
                if (++d == std::size_t(_exponent))
                    s += ".";
            } else {
                s.push_back(*p / 10 + '0');
                if (++d == std::size_t(_exponent))
                    s += ".";
                s.push_back(*p % 10 + '0');
                if (++d == std::size_t(_exponent))
                    s += ".";
            }

            p += 1;
        }
    }

    return s;
}

void DNumber::round(std::size_t n)
{
    if (_no_digits <= n + 1)
        return;

    DNumber d("5");
    d._exponent = _exponent - n - 1;
    d.normalize();


    if (_minus)
        sub(d);
    else
        add(d);

    _no_digits = n + 1;

    normalize();
}

void DNumber::add(const DNumber& b)
{
    const DNumber& a = *this;

    if (a._zero) {
        *this = b;
        return;
    }

    if (b._zero) {
        *this = a;
        return;
    }

    if (a._minus && !b._minus) {
        DNumber t(a);
        t._minus = false;
        *this = b;
        sub(t);
        return;
    }

    if (!a._minus && b._minus) {
        DNumber t(b);
        t._minus = false;
        sub(t);
        return;
    }

    _minus = a._minus;

    DNumber aa(a);
    DNumber bb(b);

    if (aa._exponent > bb._exponent) {
        aa.scale(1);
        bb.scale(aa._exponent - bb._exponent);
    } else if (aa._exponent < bb._exponent) {
        bb.scale(1);
        aa.scale(bb._exponent - aa._exponent);
    } else {
        aa.scale(1);
        bb.scale(1);
    }

    DNumber* p = &aa;
    DNumber* q = &bb;

    if (p->_no_digits < q->_no_digits)
        std::swap(p, q);

    Byte c = 0;
    for (std::size_t i = q->_rep.size(); i != 0; --i) {
        const Byte t = c + p->_rep[i - 1] + q->_rep[i - 1];
        p->_rep[i - 1] = t % 100;
        c = t / 100;
    }

    *this = *p;

    normalize();
}

void DNumber::sub(const DNumber& b)
{
    const DNumber& a = *this;

    if (a._zero) {
        *this = b;
        if (!_zero)
            _minus = !_minus;
        return;
    }

    if (b._zero) {
        *this = a;
        return;
    }

    if (a._minus && !b._minus) {
        DNumber t(b);
        t._minus = true;
        add(t);
        return;
    }

    if (!a._minus && b._minus) {
        DNumber t(b);
        t._minus = false;
        add(t);
        return;
    }

    if (a == b) {
        zero();
        return;
    }

    DNumber aa(a);
    DNumber bb(b);

    aa.abs();
    bb.abs();

    const bool swap = aa < bb;

    if (aa._exponent > bb._exponent) {
        bb.scale(aa._exponent - bb._exponent);
    } else if (aa._exponent < bb._exponent) {
        aa.scale(bb._exponent - aa._exponent);
    }

    const std::size_t no_digits =
        std::max(aa._no_digits, bb._no_digits);

    aa.expand(no_digits);
    bb.expand(no_digits);

    DNumber* p = &aa;
    DNumber* q = &bb;

    if (swap)
        std::swap(p, q);

    SignedByte c = 0;
    for (std::size_t i = q->_rep.size(); i != 0; --i) {
        const SignedByte t =
            static_cast<SignedByte>(p->_rep[i - 1]) - static_cast<SignedByte>(q->_rep[i - 1]) - c;
        if (t >= 0) {
            p->_rep[i - 1] = t;
            c = 0;
        } else {
            p->_rep[i - 1] = t + 100;
            c = 1;
        }
    }

    *this = *p;
    _minus = _minus ? !swap : swap;

    normalize();
}

void DNumber::mul(const DNumber& b)
{
    if (_zero)
        return;

    if (b._zero) {
        zero();
        return;
    }

    std::vector<Byte> data(_rep.size() + b._rep.size(), 0);
    Byte* p = &data.back();

    for (std::size_t i = _rep.size(); i != 0; --i, --p) {
        const unsigned ai = _rep[i - 1];
        for (std::size_t j = b._rep.size(); j != 0; --j) {
            const unsigned bj = b._rep[j - 1];
            const unsigned t = ai * bj;

            *p += t % 100;
            *(p - 1) += t / 100;

            if (*p >= 100) {
                *p -= 100;
                *(p - 1) += 1;
            }

            if (*(p - 1) >= 100) {
                *(p - 1) -= 100;
                *(p - 2) += 1;
            }
        }
    }

    _minus = _minus != b._minus;
    _no_digits = data.size() * 2;
    _exponent = _exponent + b._exponent;
    swap(_rep, data);

    normalize();
}

void DNumber::div(const DNumber& b, std::size_t no_places)
{
//    if (b._zero)
//        throw ...;

    if (_zero)
        return;

    if (b._no_digits == 1 && b._rep[0] == 10) {
        _minus = _minus != b._minus;
        _exponent -= b._exponent - 1;
        round(no_places);
        return;
    }

    if (*this == b) {
        *this = DNumber(1l);
        return;
    }

    DNumber aa(*this);
    DNumber bb(b);

    aa.abs();
    bb.abs();

    if (bb._rep[0] < 50) {
        const DNumber t(100l / (bb._rep[0] + 1));
        aa.mul(t);
        bb.mul(t);
    }

    _exponent = aa._exponent - bb._exponent;
    aa._exponent = 0;
    bb._exponent = 0;

    _minus = _minus != b._minus;

    if (aa == bb) {
        _exponent += 1;
        _no_digits = 1;
        _rep.resize(1);
        _rep[0] = 10;

        return;
    }

    const unsigned trial_d =
        bb._no_digits >= 3
        ? 100 * static_cast<unsigned>(bb._rep[0]) + static_cast<unsigned>(bb._rep[1])
        : 100 * static_cast<unsigned>(bb._rep[0]);

    std::size_t no_iterations =
        _exponent > 0
        ? _exponent + no_places + 1
        : no_places + 1;

    std::vector<Byte> data;

    if (aa > bb) {
        _exponent += 1;
        aa._exponent = 1;
    } else {
        aa._exponent = 2;
    }

    for (std::size_t m = 0; m < no_iterations; m += 2) {

        unsigned long trial_n = 10000l * static_cast<unsigned long>(aa._rep[0]);

        if (aa._no_digits >= 3)
            trial_n += 100l * static_cast<unsigned long>(aa._rep[1]);

        if (aa._no_digits >= 5)
            trial_n += static_cast<unsigned long>(aa._rep[2]);

        long q = trial_n / trial_d;

        switch (aa._exponent) {
        case 1:
            q /= 10;
        case 0:
            q /= 10;
        }

        if (q == 100)
            q = 99;

        DNumber qq(q);
        qq.mul(bb);

        if (qq > aa) {
            q -= 1;
            qq.sub(bb);
        }

        data.push_back(static_cast<Byte>(q));

        aa.sub(qq);

        if (aa._zero) {
            no_iterations = m + 2;
            break;
        }

        aa._exponent += 2;
    }

    _no_digits = no_iterations;
    std::swap(_rep, data);

    normalize();
}

void DNumber::dump(std::ostream& os)
{
    os << "zero: " << _zero << "\n";
    os << "minus: " << _minus << "\n";
    os << "no_digits: " << _no_digits << "\n";
    os << "size: " << _rep.size() << "\n";
    os << "exponent: " << _exponent << "\n";

    const std::ios_base::fmtflags flags = os.flags();
    const std::streamsize width = os.width();


    const std::size_t n = _rep.size();
    for (std::size_t i = 0; i < n; ++i) {
        os << std::dec << std::setw(2) << std::setfill('0') << int(_rep[i]) << ' ';
        if (i != 0 && i % 10 == 0)
            os << "\n";
    }

//    if (n == 0 || (n - 1) % 10 != 0)
        os << "\n";

    os.flags(flags);
    os.width(width);
}

void DNumber::zero()
{
    _zero = true;
    _minus = false;
    _exponent = 0;
    _rep.clear();
}

void DNumber::normalize()
{
    std::size_t n = _rep.size();

    // get rid of trailing junk
    if (n > (_no_digits + 1) / 2) {
        n = (_no_digits + 1) / 2;
        _rep.resize(n);
    }

    if (_no_digits & 1) {
        const Byte t = _rep.back() / 10;
        _rep.back() = 10 * t;
    }

    // get rid of leading zeros
    std::size_t i = 0;
    while (i < (_no_digits / 2) && _rep[i] == 0)
        i += 1;

    _no_digits -= 2 * i;
    _exponent -= 2 * i;

    if (_no_digits == 0) {
        zero();
        return;
    }

    n -= i;

    std::memmove(&_rep[0], &_rep[i], n);
    _rep.resize(n);

    if (_rep[0] / 10 == 0) {
        for (i = 0; i + 1 < n; ++i)
            _rep[i] = 10 * (_rep[i] % 10) + _rep[i + 1] / 10;
        _rep[n - 1] = 10 * (_rep[n - 1] % 10);

        _no_digits -= 1;
        _exponent -= 1;

        if (_no_digits == 0) {
            zero();
            return;
        }

        if (!(_no_digits & 1))
            _rep.resize(--n);
    }

    // get rid of trailing zeros
    i = n;
    while (i > 0 && _rep[i - 1] == 0)
        i -= 1;

    _no_digits -= 2 * (n - i);

    n = i;

    _rep.resize(n);

    if (!(_no_digits & 1) && _rep[n - 1] % 10 == 0)
        _no_digits -= 1;
}

void DNumber::scale(std::size_t n)
{
    assert(!_zero);

    if (n & 1) {
        if (!(_no_digits & 1))
            _rep.push_back(0);

        for (std::size_t i = _rep.size() - 1; i > 0; --i)
            _rep[i] = 10 * (_rep[i - 1] % 10) + _rep[i] / 10;
        _rep[0] /= 10;

        n -= 1;
        _no_digits += 1;
        _exponent += 1;
    }

    const std::size_t old_size = _rep.size();
    const std::size_t n2 = n / 2;
    _rep.resize(_rep.size() + n2);
    Byte* const p = &_rep[0];
    std::memmove(p + n2, p, old_size);
    std::memset(p, 0, n2);
    _no_digits += n;
    _exponent += n;
}

void DNumber::expand(std::size_t n)
{
    if (_no_digits >= n)
        return;

    if (_no_digits & 1) {
        Byte* p = &_rep[_rep.size() - 1] ;
        *p = (*p / 10) * 10;
    }

    _rep.resize((n + 1) / 2, 0);
    _no_digits = n;
}

// int main()
// {
//     DNumber c("9.999");
//     DNumber d("0.0011");

//     c.dump(std::cout);
//     d.dump(std::cout);

//     // std::cout << "is_integer: " << d.is_integer() << std::endl;

//     // if (d.is_integer()) {
//     //     std::cout << "is_even: " << d.is_even() << std::endl;
//     // }

//     // std::cout << d.to_string() << std::endl;

//     DNumber e;

//     e.add(c, d);

//     e.dump(std::cout);

//     std::cout << e.to_string() << std::endl;
// }
