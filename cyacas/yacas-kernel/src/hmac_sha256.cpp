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
 * File:   HMAC_SHA256.cpp
 * Author: mazur
 * 
 * Created on November 4, 2015, 11:55 AM
 */

#include "hmac_sha256.hpp"

namespace {
    static const char hex_digit[] = {
        '0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
    };
}

HMAC_SHA256::HMAC_SHA256(const std::string& key)
{
    HMAC_CTX_init(&_ctx);
    HMAC_Init_ex(&_ctx, key.c_str(), key.size(), EVP_sha256(), nullptr);
}

HMAC_SHA256::HMAC_SHA256(const std::string& key, const std::string& msg):
    HMAC_SHA256(key)
{
    update(msg);
}

HMAC_SHA256::HMAC_SHA256(const HMAC_SHA256& other)
{
    HMAC_CTX_copy(&_ctx, const_cast<HMAC_CTX*>(&other._ctx));
}

HMAC_SHA256::~HMAC_SHA256()
{
    HMAC_CTX_cleanup(&_ctx);
}


void HMAC_SHA256::update(const std::string& msg)
{
    HMAC_Update(&_ctx, (const unsigned char*)(msg.c_str()), msg.size());
}

std::string HMAC_SHA256::hexdigest()
{
    const unsigned n = 32;
    
    unsigned char result[n];
    unsigned result_len = n;
    HMAC_Final(&_ctx, result, &result_len);

    std::string s(2 * n, 0);
    for (unsigned i = 0; i < n; ++i) {
        s[2 * i] = hex_digit[result[i] / 16];
        s[2 * i + 1] = hex_digit[result[i] % 16];
    }
    
    return s;
}
