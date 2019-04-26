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

#include "yacas/mp/zz.hpp"

#include <cctype>

namespace yacas {
    namespace mp {

        const ZZ ZZ::ZERO = ZZ(0);
        const ZZ ZZ::ONE = ZZ(1);
        const ZZ ZZ::TWO = ZZ(2);
        const ZZ ZZ::TEN = ZZ(10);

        ZZ::ZZ(std::string_view s, unsigned b) : _neg(false)
        {
            auto p = s.cbegin();
            const auto q = s.cend();

            while (p != q && std::isspace(*p))
                p += 1;

            if (p == q)
                throw ParseError(s, s.length());

            if (*p == '+') {
                p += 1;
            } else if (*p == '-') {
                _neg = true;
                p += 1;
            }

            try {
                _nn = NN(std::string_view(&(*p), std::distance(p, q)), b);
            } catch (const NN::ParseError& e) {
                throw ParseError(&(*p), std::distance(p, q));
            }

            if (_nn.is_zero())
                _neg = false;
        }

        std::string ZZ::to_string(unsigned b) const
        {
            std::string s = _neg && !is_zero() ? "-" : "";
            return s + _nn.to_string(b);
        }
    }
}
