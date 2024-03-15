/*
 *
 * This file is part of yacas.
 * Yacas is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
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

#ifndef YACAS_MP_ZZ_HPP
#define YACAS_MP_ZZ_HPP

#include "nn.hpp"

namespace yacas {
    namespace mp {
        class ZZ {
        public:
            static const ZZ ZERO;
            static const ZZ ONE;
            static const ZZ TWO;
            static const ZZ TEN;

            struct ParseError : public std::invalid_argument {
                ParseError(std::string_view s, std::size_t) :
                    std::invalid_argument("yacas::mp::ZZ: error parsing " + std::string(s))
                {
                }
            };

            struct DivisionByZeroError : public std::domain_error {
                explicit DivisionByZeroError(std::string_view s) :
                    std::domain_error("yacas::mp::ZZ: attempt to divide " +
                                       std::string(s) + " by zero")
                {
                }
            };

            ZZ();
            explicit ZZ(int);
            explicit ZZ(const NN&);
            explicit ZZ(std::string_view s, unsigned b = 10);

            template <class RndEngine>
            ZZ(unsigned no_bits, RndEngine& engine);

            bool operator==(int) const;
            bool operator!=(int) const;
            bool operator<(int) const;
            bool operator>(int) const;
            bool operator<=(int) const;
            bool operator>=(int) const;

            bool operator==(const ZZ&) const;
            bool operator!=(const ZZ&) const;
            bool operator<(const ZZ&) const;
            bool operator>(const ZZ&) const;
            bool operator<=(const ZZ&) const;
            bool operator>=(const ZZ&) const;

            ZZ& operator+=(int);
            ZZ& operator-=(int);
            ZZ& operator*=(int);
            ZZ& operator/=(int);
            ZZ& operator%=(int);

            ZZ& operator+=(const ZZ&);
            ZZ& operator-=(const ZZ&);
            ZZ& operator*=(const ZZ&);
            ZZ& operator/=(const ZZ&);
            ZZ& operator%=(const ZZ&);

            ZZ& operator<<=(unsigned);
            ZZ& operator>>=(unsigned);
            ZZ& operator|=(const ZZ&);
            ZZ& operator&=(const ZZ&);
            ZZ& operator^=(const ZZ&);

            bool is_zero() const;
            bool is_positive() const;
            bool is_negative() const;
            bool is_even() const;

            void clear();

            const NN& to_NN() const;

            int to_int() const;
            std::string to_string(unsigned base = 10) const;

            void abs();
            void neg();

            void sqr();
            void pow(unsigned);

            unsigned long no_bits() const;
            unsigned long no_digits() const;

            bool test(unsigned long bit) const;
            void set(unsigned long bit);
            void clear(unsigned long bit);

        private:
            NN _nn;
            bool _neg;
        };

        inline ZZ gcd(ZZ a, ZZ b)
        {
            a.abs();
            b.abs();

            return ZZ(gcd(a.to_NN(), b.to_NN()));
        }

        inline ZZ::ZZ() : _neg(false) {}

        inline ZZ::ZZ(int i) : _nn(std::abs(i)), _neg(i < 0) {}

        inline ZZ::ZZ(const NN& n) : _nn(n), _neg(false) {}

        template <class RndEngine>
        ZZ::ZZ(unsigned no_bits, RndEngine& engine) :
            _nn(no_bits, engine),
            _neg(false)
        {
        }

        inline bool ZZ::operator==(int i) const
        {
            if (_neg == (i < 0))
                return _nn == std::abs(i);

            return false;
        }

        inline bool ZZ::operator!=(int i) const
        {
            if (_neg != (i < 0))
                return true;

            return _nn != std::abs(i);
        }

        inline bool ZZ::operator<(int i) const
        {
            if (_neg && i >= 0)
                return true;

            if (!_neg && i < 0)
                return false;

            return _nn < std::abs(i);
        }

        inline bool ZZ::operator>(int i) const
        {
            if (_neg && i >= 0)
                return false;

            if (!_neg && i < 0)
                return true;

            return _nn > std::abs(i);
        }

        inline bool ZZ::operator<=(int i) const
        {
            if (_neg && i >= 0)
                return true;

            if (!_neg && i < 0)
                return false;

            return _nn <= std::abs(i);
        }

        inline bool ZZ::operator>=(int i) const
        {
            if (_neg && i >= 0)
                return false;

            if (!_neg && i < 0)
                return true;

            return _nn >= std::abs(i);
        }

        inline bool ZZ::operator==(const ZZ& z) const
        {
            return _neg == z._neg && _nn == z._nn;
        }

        inline bool ZZ::operator!=(const ZZ& z) const
        {
            return _neg != z._neg || _nn != z._nn;
        }

        inline bool ZZ::operator<(const ZZ& z) const
        {
            if (_neg && !z._neg)
                return true;

            if (!_neg && z._neg)
                return false;

            if (_neg && z._neg)
                return z._nn < _nn;

            return _nn < z._nn;
        }

        inline bool ZZ::operator>(const ZZ& z) const { return z < *this; }

        inline bool ZZ::operator<=(const ZZ& z) const { return !(*this > z); }

        inline bool ZZ::operator>=(const ZZ& z) const { return !(*this < z); }

        inline ZZ& ZZ::operator+=(int i)
        {
            if (i == 0)
                return *this;

            if (_neg == (i < 0)) {
                _nn += std::abs(i);
                return *this;
            }

            i = std::abs(i);

            if (_nn > i) {
                _nn -= i;
            } else {
                NN t(i);
                t -= _nn;
                _nn = std::move(t);
                if (_nn.is_zero())
                    _neg = false;
                else
                    _neg = !_neg;
            }

            return *this;
        }

        inline ZZ& ZZ::operator-=(int i)
        {
            *this += -i;

            return *this;
        }

        inline ZZ& ZZ::operator*=(int i)
        {
            if (i == 0) {
                clear();
                return *this;
            }

            if (i < 0) {
                _neg = !_neg;
                i = -i;
            }
            _nn *= i;
            return *this;
        }

        inline ZZ& ZZ::operator/=(int i)
        {
            if (i == 0)
                throw DivisionByZeroError(to_string());

            if (i < 0) {
                _neg = !_neg;
                i = -i;
            }
            _nn /= i;

            if (_nn.is_zero())
                _neg = false;

            return *this;
        }

        inline ZZ& ZZ::operator%=(int i)
        {
            if (i == 0)
                throw DivisionByZeroError(to_string());

            if (i < 0) {
                _neg = !_neg;
                i = -i;
            }
            _nn %= i;

            if (_nn.is_zero())
                _neg = false;

            return *this;
        }

        inline ZZ& ZZ::operator+=(const ZZ& z)
        {
            if (_neg == z._neg) {
                _nn += z._nn;
                return *this;
            }

            if (_nn > z._nn) {
                _nn -= z._nn;
            } else {
                NN t(z._nn);
                t -= _nn;
                _nn = std::move(t);
                if (_nn.is_zero())
                    _neg = false;
                else
                    _neg = !_neg;
            }

            return *this;
        }

        inline ZZ& ZZ::operator-=(const ZZ& z)
        {
            if (_neg != z._neg) {
                _nn += z._nn;
                return *this;
            }

            if (_nn > z._nn) {
                _nn -= z._nn;
            } else {
                NN t(z._nn);
                t -= _nn;
                _nn = std::move(t);
                if (_nn.is_zero())
                    _neg = false;
                else
                    _neg = !_neg;
            }

            return *this;
        }

        inline ZZ& ZZ::operator*=(const ZZ& z)
        {
            if (is_zero())
                return *this;

            if (z.is_zero()) {
                clear();
                return *this;
            }

            if (z._neg)
                _neg = !_neg;

            _nn *= z._nn;

            return *this;
        }

        inline ZZ& ZZ::operator/=(const ZZ& z)
        {
            if (is_zero())
                return *this;

            if (z.is_zero())
                throw DivisionByZeroError(to_string());

            if (z._neg)
                _neg = !_neg;

            _nn /= z._nn;

            if (_nn.is_zero())
                _neg = false;

            return *this;
        }

        inline ZZ& ZZ::operator%=(const ZZ& z)
        {
            if (z.is_zero())
                throw DivisionByZeroError(to_string());

            if (z._neg)
                _neg = !_neg;

            _nn %= z._nn;

            if (_nn.is_zero())
                _neg = false;

            return *this;
        }

        inline ZZ& ZZ::operator<<=(unsigned n)
        {
            _nn <<= n;
            return *this;
        }

        inline ZZ& ZZ::operator>>=(unsigned n)
        {
            _nn >>= n;
            if (_nn.is_zero())
                _neg = false;
            return *this;
        }

        inline ZZ& ZZ::operator|=(const ZZ& z)
        {
            _nn |= z._nn;
            return *this;
        }

        inline ZZ& ZZ::operator&=(const ZZ& z)
        {
            _nn &= z._nn;
            if (_nn.is_zero())
                _neg = false;
            return *this;
        }

        inline ZZ& ZZ::operator^=(const ZZ& z)
        {
            _nn ^= z._nn;
            if (_nn.is_zero())
                _neg = false;
            return *this;
        }

        inline void ZZ::abs()
        {
            _neg = false;
        }

        inline void ZZ::neg()
        {
            if (!is_zero())
                _neg = !_neg;
        }

        inline void ZZ::sqr()
        {
            _neg = false;
            _nn.sqr();
        }

        inline void ZZ::pow(unsigned n)
        {
            if (n % 2 == 0)
                _neg = false;

            _nn.pow(n);
        }

        inline unsigned long ZZ::no_bits() const { return _nn.no_bits(); }

        inline unsigned long ZZ::no_digits() const { return _nn.no_digits(); }

        inline bool ZZ::test(unsigned long bit) const { return _nn.test(bit); }

        inline void ZZ::set(unsigned long bit) { _nn.set(bit); }

        inline void ZZ::clear(unsigned long bit)
        {
            _nn.clear(bit);

            if (_nn.is_zero())
                _neg = false;
        }

        inline bool ZZ::is_zero() const { return _nn.is_zero(); }

        inline bool ZZ::is_positive() const
        {
            if (is_zero())
                return false;

            return !_neg;
        }

        inline bool ZZ::is_negative() const { return _neg; }

        inline bool ZZ::is_even() const { return _nn.is_even(); }

        inline void ZZ::clear()
        {
            _nn.clear();
            _neg = false;
        }

        inline const NN& ZZ::to_NN() const
        {
            assert(!_neg);

            return _nn;
        }

        inline int ZZ::to_int() const
        {
            return (is_negative() ? -1 : 1) *
                   static_cast<int>(_nn.to_unsigned());
        }

        inline ::std::ostream& operator<<(::std::ostream& os, const ZZ& z)
        {
            unsigned base = 10;
            if (os.flags() & std::ios::hex)
                base = 16;
            if (os.flags() & std::ios::oct)
                base = 8;

            return os << z.to_string(base);
        }

        inline ::std::istream& operator>>(::std::istream& os, ZZ& z)
        {
            unsigned base = 10;
            if (os.flags() & std::ios::hex)
                base = 16;
            if (os.flags() & std::ios::oct)
                base = 8;

            std::string s;
            os >> s;
            z = ZZ(s, base);

            return os;
        }
    }
}

#endif
