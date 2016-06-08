/*
 * This file is part of yacas_kernel.
 * Yacas_kernel is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesset General Public License as
 * published by the Free Software Foundation, either version 2.1
 * of the License, or (at your option) any later version.
 *
 * Yacas_kernel is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with yacas_kernel.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/* 
 * File:   hmac_sha256.hpp
 * Author: mazur
 *
 * Created on November 4, 2015, 11:55 AM
 */

#ifndef HMAC_SHA256_HPP
#define HMAC_SHA256_HPP

#include <string>
#include <openssl/hmac.h>

class HMAC_SHA256 {
public:
    explicit HMAC_SHA256(const std::string& key);
    HMAC_SHA256(const std::string& key, const std::string& msg);
    HMAC_SHA256(const HMAC_SHA256&);
    ~HMAC_SHA256();

    void update(const std::string&);

    std::string hexdigest();

private:
    HMAC_CTX _ctx;
};

#endif
