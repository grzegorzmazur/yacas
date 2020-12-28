/*
 *
 * This file is part of yacas.
 * Yacas is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesset General Public License as
 * published by the Free Software Foundation, either version 2.1
 * of the License, or (at your option) any later version.
 *
 * Yacas is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with yacas.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "yacas/mp/nn.hpp"

#include <cctype>

namespace {
    using namespace yacas::mp;

    typedef NN::Limb Limb;
    typedef NN::Limb2 Limb2;

    static constexpr int LIMB_BITS = sizeof(Limb) * CHAR_BIT;

    void _mul(const Limb* __restrict p, unsigned n, Limb a, Limb* __restrict r)
    {
        if (n == 1) {
            const Limb2 v = static_cast<Limb2>(*p) * a;
            r[0] = static_cast<Limb>(v);
            r[1] = static_cast<Limb>(v >> LIMB_BITS);
            return;
        }

        Limb carry = 0;

        for (unsigned j = 0; j < n; ++j) {
            const Limb2 v = static_cast<Limb2>(*p++) * a + carry;
            carry = static_cast<Limb>(v >> LIMB_BITS);
            *r += static_cast<Limb>(v);
            carry += *r++ < static_cast<Limb>(v);
        }

        while (carry) {
            const Limb v = *r + carry;
            carry = v < *r;
            *r++ = v;
        }
    }

    bool ssub(NN& a, const NN& b)
    {
        if (a >= b) {
            a -= b;
            return true;
        }

        NN t(a);
        a = b;
        a -= t;

        return false;
    }

    void sadd(NN& a, bool& ap, const NN& b, bool bp)
    {
        if (ap == bp) {
            a += b;
            return;
        }

        if (ap && !bp) {
            ap = ssub(a, b);
            return;
        }

        NN t(a);
        a = b;
        ap = ssub(a, t);
    }

    void ssub(NN& a, bool& ap, const NN& b, bool bp)
    {
        sadd(a, ap, b, !bp);
        return;
    }

    const Limb2 log2x2to31[] = {
        2147483648, 1354911328, 1073741824, 924870866, 830760077, 764949109,
        715827882,  677455664,  646456993,  620761987, 599025414, 580332017,
        564035581,  549665672,  536870912,  525383038, 514993350, 505536792,
        496880929,  488918136,  481559945,  474732891, 468375400, 462435433,
        456868671,  451637109,  446707947,  442052706, 437646531, 433467612,
        429496729,  425716864,  422112891,  418671311, 415380038};
}

namespace yacas {
    namespace mp {

        const NN NN::ZERO = NN(0u);
        const NN NN::ONE = NN(1u);
        const NN NN::TWO = NN(2u);
        const NN NN::TEN = NN(10u);

        unsigned NN::MUL_TOOM22_THRESHOLD = 32;
        unsigned NN::MUL_TOOM33_THRESHOLD = 48;

        unsigned NN::PARSE_DC_THRESHOLD = 512;
        unsigned NN::TO_STRING_DC_THRESHOLD = 24;
        unsigned NN::DIV_REM_DC_THRESHOLD = 4;

        NN::NN(std::string_view s, unsigned b)
        {
            auto p = s.cbegin();
            const auto q = s.cend();

            while (p != q && std::isspace(*p))
                p += 1;

            if (p == q)
                throw ParseError(s, s.length());

            while (p != q && std::isalnum(*p)) {
                const char c = *p++;
                const Limb d = std::isdigit(c) ? Limb(c - '0') : Limb((c | 0x20) - 'a' + 10);

                if (d >= b)
                    throw ParseError(s, std::distance(s.cbegin(), q));

                mul(b);
                add(d);
            }

            drop_zeros();
        }

        std::string NN::to_string(unsigned base) const
        {
            assert(base > 1);
            assert(base <= 36);

            return _limbs.size() < TO_STRING_DC_THRESHOLD ? to_string_bc(base)
                                                          : to_string_dc(base);
        }

        std::string NN::to_string_bc(unsigned base) const
        {
            assert(base > 1);
            assert(base <= 36);

            if (_limbs.empty())
                return "0";

            if (base == 10 && _limbs.size() == 1)
                return std::to_string(_limbs.back());

            if (base == 10 && _limbs.size() == 2)
                return std::to_string(
                    (static_cast<Limb2>(_limbs.back()) << LIMB_BITS) +
                    _limbs.front());

            NN t(*this);
            std::string s;

            while (!t._limbs.empty()) {
                const Limb r = t.div_rem(base);
                s += r <= 9 ? r + '0' : r - 10 + 'a';
            }

            std::reverse(s.begin(), s.end());

            return s;
        }

        std::string NN::to_string_dc(unsigned base) const
        {
            assert(base > 1);
            assert(base <= 36);

            if (_limbs.size() < 3)
                return to_string(base);

            const unsigned k = (no_bits() * log2x2to31[base - 2]) >> 32;

            NN Bk(base);
            Bk.pow(k);

            NN Q(*this);
            const NN R = Q.div_rem(Bk);

            const std::string r = R.to_string(base);
            return Q.to_string(base) + std::string(k - r.length(), '0') + r;
        }

        void NN::shift_left(unsigned n)
        {
            if (n >= LIMB_BITS) {
                _limbs.insert(_limbs.begin(), n / LIMB_BITS, 0);
                n %= LIMB_BITS;
            }

            if (!n)
                return;

            Limb c = 0;
            for (Limbs::iterator i = _limbs.begin(); i != _limbs.end(); ++i) {
                const Limb2 t = static_cast<Limb2>(*i) << n;
                *i = static_cast<Limb>(t) + c;
                c = static_cast<Limb>(t >> LIMB_BITS);
            }

            if (c)
                _limbs.push_back(c);
        }

        void NN::shift_right(unsigned n)
        {
            if (n >= LIMB_BITS) {
                _limbs.erase(_limbs.begin(), _limbs.begin() + n / LIMB_BITS);
                n %= LIMB_BITS;
            }

            if (!n)
                return;

            Limb c = 0;
            for (Limbs::reverse_iterator i = _limbs.rbegin();
                 i != _limbs.rend();
                 ++i) {
                const Limb t = *i << (LIMB_BITS - n);
                *i = (*i >> n) + c;
                c = t;
            }

            drop_zeros();
        }

        void NN::add(Limb a)
        {
            if (a == 0)
                return;

            if (_limbs.empty()) {
                _limbs.push_back(a);
                return;
            }

            _limbs.push_back(0);

            Limb* __restrict p = _limbs.data();

            Limb v = *p + a;
            Limb carry = v < *p;
            *p++ = v;

            while (carry) {
                v = *p + carry;
                carry = v < *p;
                *p++ = v;
            }

            drop_zeros();
        }

        void NN::sub(Limb a)
        {
            if (a == 0)
                return;

            assert(!_limbs.empty());

            Limb* __restrict p = _limbs.data();

            Limb v = *p - a;
            Limb borrow = v > *p;
            *p++ = v;

            while (borrow) {

                assert(p < _limbs.data() + _limbs.size());

                const Limb v = *p - borrow;
                borrow = v > *p;
                *p++ = v;
            }

            drop_zeros();
        }

        void NN::mul(Limb b)
        {
            if (_limbs.empty())
                return;

            if (b == 0) {
                _limbs.clear();
                return;
            }

            const unsigned n = static_cast<unsigned>(_limbs.size());
            _limbs.push_back(0);
            Limb* __restrict p = _limbs.data();

            Limb carry = 0;

            for (unsigned i = 0; i < n; ++i) {
                const Limb2 v = static_cast<Limb2>(*p) * b + carry;
                carry = static_cast<Limb>(v >> LIMB_BITS);
                *p++ = static_cast<Limb>(v);
            }

            while (carry) {
                const Limb v = *p + carry;
                carry = v < *p;
                *p++ = v;
            }

            drop_zeros();
        }

        void NN::div(Limb a) { div_rem(a); }

        void NN::rem(Limb a)
        {
            _limbs = {div_rem(a)};
            drop_zeros();
        }

        NN::Limb NN::div_rem(Limb a)
        {
            if (a == 0)
                throw DivisionByZeroError(to_string());

            const unsigned n = static_cast<unsigned>(_limbs.size());
            Limbs q(n);

            Limb2 t = 0;

            const Limb* __restrict p = _limbs.data() + n - 1;
            Limb* __restrict qp = q.data() + n - 1;

            for (unsigned i = 0; i < n; ++i) {
                t <<= LIMB_BITS;
                t += *p--;
                *qp-- = static_cast<Limb>(t / a);
                t %= a;
            }

            _limbs = std::move(q);
            drop_zeros();

            return static_cast<Limb>(t);
        }

        void NN::add(const NN& a, unsigned shift)
        {
            if (this == &a) {
                if (shift == 0)
                    shift_left(1);
                else
                    add(NN(a), shift);
                return;
            }

            if (a.is_zero())
                return;

            if (is_zero()) {
                _limbs = a._limbs;
                shift_left(shift);
                return;
            }

            const std::size_t na = a._limbs.size();

            if (na + shift > _limbs.size())
                _limbs.resize(na + shift + 1, 0);
            else
                _limbs.push_back(0);

            Limb* __restrict p = _limbs.data() + shift;
            const Limb* __restrict q = a._limbs.data();

            Limb carry = 0;

            for (unsigned i = 0; i < na; ++i) {
                const Limb v = *p + *q++ + carry;
                carry = v < *p;
                *p++ = v;
                assert(p <= _limbs.data() + _limbs.size());
            }

            while (carry) {
                const Limb v = *p + carry;
                carry = v < *p;
                *p++ = v;
                assert(p <= _limbs.data() + _limbs.size());
            }

            drop_zeros();
        }

        void NN::sub(const NN& a, unsigned shift)
        {
#ifndef NDEBUG
            NN aa(a);
            aa.shift_left(shift);
            assert(*this >= aa);
#endif
            if (a.is_zero())
                return;

            if (this == &a) {
                assert(shift == 0);
                clear();
                return;
            }

            const std::size_t na = a._limbs.size();

            if (na + shift > _limbs.size())
                _limbs.resize(na + shift + 1, 0);
            else
                _limbs.push_back(0);

            Limb* __restrict p = _limbs.data() + shift;
            const Limb* __restrict q = a._limbs.data();

            Limb borrow = 0;

            for (unsigned i = 0; i < na; ++i) {
                const Limb v = *p - *q++ - borrow;
                borrow = v > *p;
                *p++ = v;
                assert(p <= _limbs.data() + _limbs.size());
            }

            while (borrow) {
                const Limb v = *p - borrow;
                borrow = v > *p;
                *p++ = v;
                assert(p <= _limbs.data() + _limbs.size());
            }

            drop_zeros();
        }

        void NN::mul(const NN& a) { mul_bc(a); }

        void NN::mul_bc(const NN& a)
        {
            const unsigned m = static_cast<unsigned>(_limbs.size());
            const unsigned n = static_cast<unsigned>(a._limbs.size());

            Limbs result(m + n, 0);

            Limb* __restrict r = result.data();

            if (m >= n) {
                const Limb* __restrict p = _limbs.data();
                for (unsigned i = 0; i < n; ++i)
                    if (const Limb u = a._limbs[i])
                        _mul(p, m, u, r + i);
            } else {
                const Limb* __restrict q = a._limbs.data();
                for (unsigned i = 0; i < m; ++i)
                    if (const Limb u = _limbs[i])
                        _mul(q, n, u, r + i);
            }

            _limbs = std::move(result);

            drop_zeros();
        }

        void NN::sqr()
        {
            const unsigned n = static_cast<unsigned>(_limbs.size());

            if (n < MUL_TOOM22_THRESHOLD)
                sqr_bc();
            else if (n < MUL_TOOM33_THRESHOLD)
                sqr_toom22();
            else
                sqr_toom33();
        }

        void NN::sqr_bc()
        {
            if (_limbs.empty())
                return;

            const unsigned n = static_cast<unsigned>(_limbs.size());

            Limbs c(2 * n, 0);

            const Limb* __restrict p = _limbs.data();
            Limb* __restrict r = c.data();

            for (unsigned i = 0; i < n; ++i)
                if (const Limb u = p[i])
                    _mul(p, n, u, r + i);

            _limbs = std::move(c);

            drop_zeros();
        }

        void NN::sqr_toom22()
        {
            const unsigned n = static_cast<unsigned>(_limbs.size());

            assert(n >= 2);

            const unsigned k = n / 2;

            NN x0, x1;

            x1._limbs.assign(_limbs.begin(), _limbs.begin() + k);
            x1.drop_zeros();
            x0._limbs.assign(_limbs.begin() + k, _limbs.end());
            x0.drop_zeros();

            NN d;
            if (x0 < x1) {
                d = x1;
                d.sub(x0);
            } else {
                d = x0;
                d.sub(x1);
            }

            d.sqr();
            x0.sqr();
            x1.sqr();

            _limbs = x1._limbs;
            x1.add(x0);
            add(x1, k);
            add(x0, 2 * k);
            sub(d, k);
        }

        void NN::sqr_toom33()
        {
            const unsigned n = static_cast<unsigned>(_limbs.size());

            assert(n >= 3);

            const unsigned k = (n + 1) / 3;

            NN x0, x1, x2;

            x0._limbs.assign(_limbs.begin(), _limbs.begin() + k);
            x0.drop_zeros();
            x1._limbs.assign(_limbs.begin() + k, _limbs.begin() + 2 * k);
            x1.drop_zeros();
            x2._limbs.assign(_limbs.begin() + 2 * k, _limbs.end());
            x2.drop_zeros();

            // p0 <- x0
            NN p0(x0);

            // t <- x0 + x2
            NN t(x0);
            t += x2;

            // p1 <- t + x1
            NN p1(t);
            p1 += x1;

            // p_1 <- t - x1
            NN p_1(t);
            bool p_1p = ssub(p_1, x1);

            // p_2 <- (p_1 + x2) * 2 − x0
            NN p_2(p_1);
            bool p_2p = p_1p;
            sadd(p_2, p_2p, x2, true);
            p_2.shift_left(1);
            ssub(p_2, p_2p, x0, true);

            NN pi(x2);

            pi.sqr();
            p_2.sqr();
            p1.sqr();
            p_1.sqr();
            p0.sqr();

            NN r0(p0);
            NN r4(pi);

            // r3 <- (p_2 - p1) / 3
            NN r3(p_2);
            bool r3p = ssub(r3, p1);
            r3.div(3);

            // r1 <- (p1 - p_1) / 2
            NN r1(p1);
            bool r1p = ssub(r1, p_1);
            r1.shift_right(1);

            // r2 <- p_1 - r0
            NN r2(p_1);
            bool r2p = ssub(r2, r0);

            // r3 <- (r2 − r3)/2 + 2 * pi
            r3p = !r3p;
            sadd(r3, r3p, r2, r2p);
            r3.shift_right(1);
            pi.shift_left(1);
            sadd(r3, r3p, pi, true);

            // r2 <- r2 + r1 - r4
            sadd(r2, r2p, r1, r1p);
            ssub(r2, r2p, r4, true);

            // r1 <- r1 - r3
            ssub(r1, r1p, r3, r3p);

            _limbs = r0._limbs;

            if (r1p)
                add(r1, k);
            if (r2p)
                add(r2, 2 * k);
            if (r3p)
                add(r3, 3 * k);

            add(r4, 4 * k);

            if (!r1p)
                sub(r1, k);
            if (!r2p)
                sub(r2, 2 * k);
            if (!r3p)
                sub(r3, 3 * k);
        }

        void NN::pow(unsigned n)
        {
            NN a(ONE);
            _limbs.swap(a._limbs);

            while (n) {
                if (n % 2) {
                    mul(a);
                    n -= 1;
                }
                a.sqr();
                n /= 2;
            }
        }

        void NN::div(const NN& d) { div_rem(d); }

        void NN::rem(const NN& d) { _limbs = std::move(div_rem(d)._limbs); }

        NN NN::div_rem(const NN& d)
        {
            if (d.is_zero())
                throw DivisionByZeroError(to_string());

            if (*this < d) {
                NN r;
                r._limbs.swap(_limbs);
                return r;
            }

            if (*this == d) {
                *this = ONE;
                return ZERO;
            }

            return div_rem_bc(d);
        }

        NN NN::div_rem_bc(const NN& d)
        {
            assert(!d.is_zero());

            NN A(*this);
            NN B(d);

#ifdef _MSC_VER
            unsigned long index = 0;
            _BitScanReverse(&index, B._limbs.back());
            const unsigned k = 31 - index;
#else
            const unsigned k = __builtin_clz(B._limbs.back());
#endif

            B <<= k;
            A <<= k;

            const unsigned n = static_cast<unsigned>(B._limbs.size());
            const unsigned m = static_cast<unsigned>(A._limbs.size() - n);

            B._limbs.insert(B._limbs.begin(), m, 0);

            _limbs.clear();
            if (A >= B) {
                _limbs.resize(m + 1, 0);
                _limbs[m] = 1;
                A -= B;
            } else {
                _limbs.resize(m, 0);
            }

            if (A.is_zero()) {
                drop_zeros();
                return ZERO;
            }

            const Limb B_leading_digit = B._limbs.back();

            for (unsigned jj = 0; jj < m; ++jj) {
                const unsigned j = m - 1 - jj;

                Limb2 qs = 0;
                if (n + j < A._limbs.size()) {
                    qs = A._limbs[n + j];
                    qs <<= LIMB_BITS;

                    if (n + j < A._limbs.size() + 1)
                        qs += A._limbs[n + j - 1];
                }

                qs /= B_leading_digit;

                _limbs[j] = static_cast<Limb>(std::min(qs, static_cast<Limb2>(LIMB_MAX)));

                NN T;

                for (;;) {
                    T._limbs.assign(B._limbs.begin() + jj + 1, B._limbs.end());
                    T *= _limbs[j];

                    if (A >= T)
                        break;

                    _limbs[j] -= 1;
                }

                A -= T;
            }

            drop_zeros();

            A >>= k;

            return A;
        }

        NN gcd(NN a, NN b)
        {
            NN t;

            while (!b.is_zero()) {
                t = b;
                b = a;
                b %= t;
                a = t;
            }

            return a;
        }
    }
}
