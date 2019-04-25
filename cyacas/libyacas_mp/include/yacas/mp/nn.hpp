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

#ifndef YACAS_MP_NN_HPP
#define YACAS_MP_NN_HPP

#include <algorithm>
#include <cassert>
#include <climits>
#include <iostream>
#include <random>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace yacas {
    namespace mp {
        class NN {
        public:
            typedef std::uint32_t Limb;
            typedef std::uint64_t Limb2;

            static const NN ZERO;
            static const NN ONE;
            static const NN TWO;
            static const NN TEN;

            static unsigned PARSE_DC_THRESHOLD;
            static unsigned TO_STRING_DC_THRESHOLD;
            static unsigned DIV_REM_DC_THRESHOLD;

            static unsigned MUL_TOOM22_THRESHOLD;
            static unsigned MUL_TOOM33_THRESHOLD;

            struct ParseError : public std::invalid_argument {
                ParseError(std::string_view s, std::size_t) :
                    std::invalid_argument("yacas::mp::NN: error parsing " +
                                          std::string(s))
                {
                }
            };

            struct DivisionByZeroError : public std::domain_error {
                DivisionByZeroError(std::string_view s) :
                    std::domain_error("yacas::mp::NN: attempt to divide " +
                                      std::string(s) + " by zero")
                {
                }
            };

            NN() = default;

            explicit NN(Limb);
            explicit NN(const std::vector<Limb>&);

            explicit NN(std::string_view, unsigned b = 10);

            template <class RndEngine> NN(unsigned no_bits, RndEngine& engine);

            bool operator==(Limb) const;
            bool operator!=(Limb) const;
            bool operator<(Limb) const;
            bool operator>(Limb) const;
            bool operator<=(Limb) const;
            bool operator>=(Limb) const;

            bool operator==(const NN&) const;
            bool operator!=(const NN&) const;
            bool operator<(const NN&) const;
            bool operator>(const NN&) const;
            bool operator<=(const NN&) const;
            bool operator>=(const NN&) const;

            NN& operator+=(Limb);
            NN& operator-=(Limb);
            NN& operator*=(Limb);
            NN& operator/=(Limb);
            NN& operator%=(Limb);

            NN& operator+=(const NN&);
            NN& operator-=(const NN&);
            NN& operator*=(const NN&);
            NN& operator/=(const NN&);
            NN& operator%=(const NN&);

            NN& operator<<=(unsigned);
            NN& operator>>=(unsigned);
            NN& operator|=(const NN&);
            NN& operator&=(const NN&);
            NN& operator^=(const NN&);

            bool is_zero() const;
            bool is_even() const;

            void clear();

            unsigned to_unsigned() const;
            std::string to_string(unsigned base = 10) const;

            void sqr();
            void pow(unsigned);

            unsigned long no_bits() const;
            unsigned long no_digits() const;

            bool test(unsigned long bit) const;
            void set(unsigned long bit);
            void clear(unsigned long bit);

            const std::vector<Limb>& limbs() const;

        private:
            static constexpr int LIMB_BITS = sizeof(Limb) * CHAR_BIT;
            static constexpr int LIMB2_BITS = sizeof(Limb2) * CHAR_BIT;

            static constexpr Limb LIMB_MAX = std::numeric_limits<Limb>::max();

            static constexpr Limb2 BASE = static_cast<Limb2>(LIMB_MAX) + 1;

            typedef std::vector<Limb> Limbs;

            Limbs _limbs;

            template <typename Iter> NN(Iter b, Iter e)
            {
                std::copy(b, e, std::back_inserter(_limbs));
                drop_zeros();
            }

            void drop_zeros();

            void add(Limb);
            void sub(Limb);
            void mul(Limb);
            void div(Limb);
            void rem(Limb);
            Limb div_rem(Limb);

            void shift_left(unsigned n);
            void shift_right(unsigned n);

            void add(const NN&, unsigned shift = 0);
            void sub(const NN&, unsigned shift = 0);
            void mul(const NN&);
            void div(const NN&);
            void rem(const NN&);
            NN div_rem(const NN&);

            void sqr_bc();
            void sqr_toom22();
            void sqr_toom33();

            void mul_bc(const NN&);
            void mul_toom22(const NN&);
            void mul_toom33(const NN&);

            NN div_rem_bc(const NN&);

            std::string to_string_bc(unsigned base = 10) const;
            std::string to_string_dc(unsigned base = 10) const;
        };

        NN gcd(NN a, NN b);

        inline NN::NN(Limb n)
        {
            if (n != 0)
                _limbs.push_back(n);
        }

        inline NN::NN(const std::vector<Limb>& limbs) : _limbs(limbs)
        {
            drop_zeros();
        }

        template <class RndEngine> NN::NN(unsigned no_bits, RndEngine& engine)
        {
            const unsigned no_limbs =
                ((unsigned long)no_bits + LIMB_BITS - 1) / LIMB_BITS;

            const unsigned no_full_limbs = no_bits / LIMB_BITS;

            _limbs.reserve(no_limbs);

            std::uniform_int_distribution<Limb> distribution;

            for (unsigned i = 0; i < no_full_limbs; ++i)
                _limbs.push_back(distribution(engine));

            const unsigned excess_bits = no_limbs * LIMB_BITS - no_bits;

            if (excess_bits) {
                distribution = std::uniform_int_distribution<Limb>(
                    0, static_cast<Limb>(1) << (excess_bits - 1));
                _limbs.push_back(distribution(engine));
            }

            drop_zeros();
        }

        inline bool NN::operator==(Limb n) const
        {
            return _limbs.size() == 1 && _limbs.front() == n;
        }

        inline bool NN::operator!=(Limb n) const
        {
            return _limbs.size() != 1 || _limbs.front() != n;
        }

        inline bool NN::operator<(Limb n) const
        {
            return _limbs.empty() || (_limbs.size() == 1 && _limbs.front() < n);
        }

        inline bool NN::operator>(Limb n) const
        {
            return _limbs.size() > 1 ||
                   (_limbs.size() == 1 && _limbs.front() > n);
        }

        inline bool NN::operator<=(Limb n) const { return !(*this > n); }

        inline bool NN::operator>=(Limb n) const { return !(*this < n); }

        inline bool NN::operator==(const NN& n) const
        {
            return _limbs == n._limbs;
        }

        inline bool NN::operator!=(const NN& n) const
        {
            return _limbs != n._limbs;
        }

        inline bool NN::operator<(const NN& n) const
        {
            if (_limbs.size() < n._limbs.size())
                return true;

            if (_limbs.size() > n._limbs.size())
                return false;

            return std::lexicographical_compare(_limbs.rbegin(),
                                                _limbs.rend(),
                                                n._limbs.rbegin(),
                                                n._limbs.rend());
        }

        inline bool NN::operator>(const NN& n) const { return n < *this; }

        inline bool NN::operator<=(const NN& n) const { return !(*this > n); }

        inline bool NN::operator>=(const NN& n) const { return !(*this < n); }

        inline NN& NN::operator+=(Limb n)
        {
            add(n);
            return *this;
        }

        inline NN& NN::operator-=(Limb n)
        {
            sub(n);
            return *this;
        }

        inline NN& NN::operator*=(Limb n)
        {
            mul(n);
            return *this;
        }

        inline NN& NN::operator/=(Limb n)
        {
            div_rem(n);
            return *this;
        }

        inline NN& NN::operator%=(Limb n)
        {
            _limbs = {div_rem(n)};
            drop_zeros();
            return *this;
        }

        inline NN& NN::operator+=(const NN& n)
        {
            add(n);
            return *this;
        }

        inline NN& NN::operator-=(const NN& n)
        {
            sub(n);
            return *this;
        }

        inline NN& NN::operator*=(const NN& n)
        {
            mul(n);
            return *this;
        }

        inline NN& NN::operator/=(const NN& n)
        {
            div(n);
            return *this;
        }

        inline NN& NN::operator%=(const NN& n)
        {
            rem(n);
            return *this;
        }

        inline NN& NN::operator<<=(unsigned n)
        {
            shift_left(n);
            return *this;
        }

        inline NN& NN::operator>>=(unsigned n)
        {
            shift_right(n);
            return *this;
        }

        inline NN& NN::operator|=(const NN& b)
        {
            const unsigned n = b._limbs.size();

            if (_limbs.size() < n)
                _limbs.resize(n, 0);

            Limb* __restrict p = _limbs.data();
            const Limb* __restrict q = b._limbs.data();

            for (unsigned i = 0; i < n; ++i)
                *p++ |= *q++;

            return *this;
        }

        inline NN& NN::operator&=(const NN& b)
        {
            const unsigned n = b._limbs.size();

            if (_limbs.size() > n)
                _limbs.resize(n);

            const unsigned m = _limbs.size();

            Limb* __restrict p = _limbs.data();
            const Limb* __restrict q = b._limbs.data();

            for (unsigned i = 0; i < m; ++i)
                *p++ &= *q++;

            drop_zeros();

            return *this;
        }

        inline NN& NN::operator^=(const NN& b)
        {
            const unsigned n = b._limbs.size();

            if (_limbs.size() > n)
                _limbs.resize(n);

            const unsigned m = _limbs.size();

            Limb* __restrict p = _limbs.data();
            const Limb* __restrict q = b._limbs.data();

            for (unsigned i = 0; i < m; ++i)
                *p++ ^= *q++;

            drop_zeros();

            return *this;
        }

        inline unsigned long NN::no_bits() const
        {
            if (is_zero())
                return 1;

#ifdef _MSC_VER
            Limb index = 0;
            _BitScanReverse(&index, _limbs.back());
            return _limbs.size() * LIMB_BITS - (31 - index);
#else
            return _limbs.size() * LIMB_BITS - __builtin_clz(_limbs.back());
#endif
        }

        inline unsigned long NN::no_digits() const
        {
            if (is_zero())
                return 1;

            const unsigned n = ((no_bits() + 1) * 646456993) >> 31;

            NN t = TEN;
            t.pow(n);

            return *this < t ? n : n + 1;
        }

        inline bool NN::test(unsigned long bit) const
        {
            assert(bit < _limbs.size() * LIMB_BITS);
            return _limbs[bit / LIMB_BITS] & (1 << (bit % LIMB_BITS));
        }

        inline void NN::set(unsigned long bit)
        {
            assert(bit < _limbs.size() * LIMB_BITS);
            _limbs[bit / LIMB_BITS] |= (1 << (bit % LIMB_BITS));
        }

        inline void NN::clear(unsigned long bit)
        {
            assert(bit < _limbs.size() * LIMB_BITS);
            _limbs[bit / LIMB_BITS] &= ~(1 << (bit % LIMB_BITS));
        }

        inline bool NN::is_zero() const { return _limbs.empty(); }

        inline bool NN::is_even() const
        {
            if (is_zero())
                return true;

            return !(_limbs.front() & 1);
        }

        inline void NN::clear() { _limbs.clear(); }

        inline unsigned NN::to_unsigned() const
        {
            if (is_zero())
                return 0;

            return _limbs.front();
        }

        inline void NN::drop_zeros()
        {
            while (!_limbs.empty() && (_limbs.back() == 0))
                _limbs.pop_back();
        }

        inline const std::vector<NN::Limb>& NN::limbs() const { return _limbs; }

        inline ::std::ostream& operator<<(::std::ostream& os, const NN& n)
        {
            unsigned base = 10;
            if (os.flags() & std::ios::hex)
                base = 16;
            if (os.flags() & std::ios::oct)
                base = 8;

            return os << n.to_string(base);
        }

        inline ::std::istream& operator>>(::std::istream& os, NN& n)
        {
            unsigned base = 10;
            if (os.flags() & std::ios::hex)
                base = 16;
            if (os.flags() & std::ios::oct)
                base = 8;

            std::string s;
            os >> s;
            n = NN(s, base);

            return os;
        }
    }
}

#endif
