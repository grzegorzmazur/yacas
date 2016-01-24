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
 * License along with yacas.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/* 
 * File:   js_interface.cpp
 * Author: mazur
 *
 * Created on January 19, 2016, 10:34 AM
 */

#include <emscripten.h>

#include <string>

#include "yacas/yacas.h"

namespace {
    static CYacas* _yacas;
    static std::string _yacas_result;
    static std::string _yacas_side_effects;
    static bool _yacas_is_error;
}

extern "C" bool EMSCRIPTEN_KEEPALIVE yacas_is_error()
{
    return _yacas_is_error;
}

extern "C" char* EMSCRIPTEN_KEEPALIVE yacas_result()
{
    std::size_t rn = _yacas_result.length();
    char* r = new char[rn + 1];
    for (std::size_t i = 0; i < rn; ++i)
        r[i] = _yacas_result[i];

    r[rn] = 0;

    return r;
}

extern "C" char* EMSCRIPTEN_KEEPALIVE yacas_side_effects()
{
    std::size_t rn = _yacas_side_effects.length();
    char* r = new char[rn + 1];
    for (std::size_t i = 0; i < rn; ++i)
        r[i] = _yacas_side_effects[i];

    r[rn] = 0;

    return r;
}

extern "C" void EMSCRIPTEN_KEEPALIVE yacas_evaluate(const char* const p)
{
    static std::stringstream os;
    os.clear();
    os.str("");

    if (!_yacas) {
        _yacas = new CYacas(os);
        _yacas->Evaluate("DefaultDirectory(\"/share/yacas/scripts/\");");
        _yacas->Evaluate("Load(\"yacasinit.ys\");");
    }

    _yacas->Evaluate(p);

    _yacas_is_error = _yacas->IsError();

    if (!_yacas_is_error)
        _yacas_result = _yacas->Result();
    else
        _yacas_result = _yacas->Error();

    _yacas_side_effects = os.str();
}
