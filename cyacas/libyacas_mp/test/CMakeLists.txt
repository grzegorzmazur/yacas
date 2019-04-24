#
# Copyright (C) 2017 Grzegorz Mazur <teoretyk@gmail.com>
#
# This file is part of yacas.
# Yacas is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesset General Public License as
# published by the Free Software Foundation, either version 2.1
# of the License, or (at your option) any later version.
#
# Yacas is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with yacas.  If not, see <http://www.gnu.org/licenses/>.
#
#

find_package (GTest REQUIRED)

add_executable (yacas_mp_test src/nn_test.cpp src/zz_test.cpp)
target_link_libraries (yacas_mp_test libyacas_mp GTest::GTest GTest::Main)

gtest_add_tests (yacas_mp_test "" AUTO)
