/*
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
 * License along with yacas. If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include "base64.hpp"

#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/insert_linebreaks.hpp>
#include <boost/archive/iterators/transform_width.hpp>

std::string base64_encode(std::vector<unsigned char> data)
{
    using namespace boost::archive::iterators;

    typedef insert_linebreaks< // insert line breaks every 72 characters
        base64_from_binary<    // convert binary values ot base64 characters
            transform_width< // retrieve 6 bit integers from a sequence of 8 bit
                             // bytes
                const char*,
                6,
                8>>,
        72>
        base64_text; // compose all the above operations in to a new iterator

    int p = 0;
    while (data.size() % 3) {
        data.push_back(0);
        p += 1;
    }

    std::string result(base64_text(&data.front()), base64_text(&data.front() + data.size() - p));

    for (int i = 0; i < p; ++i)
        result.push_back('=');

    return result;
}