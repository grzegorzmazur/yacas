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
 * File:   main.cpp
 * Author: mazur
 *
 * Created on November 8, 2015, 10:34 AM
 */

#include "yacas_kernel.hpp"
#include <jsoncpp/json/json.h>

#include <iostream>

int main(int argc, char** argv)
{
    if (argc < 2 || argc > 3) {
        std::cerr << "yacas_kernel: wrong number of arguments\n";
        return 1;
    }
    
    Json::Value config;

    {
        std::ifstream config_file(argv[1]);
        config_file >> config;
    }
    
    std::string scripts_path = "/usr/share/yacas/scripts/";
    
    if (argc == 3)
        scripts_path = argv[2];
    
    if (scripts_path.back() != '/')
        scripts_path.push_back('/');
    
    YacasKernel kernel(scripts_path, config);
    
    kernel.run();
}
