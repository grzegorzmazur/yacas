#
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

add_library (libyacas_mp src/nn.cpp src/zz.cpp)
target_include_directories (libyacas_mp PUBLIC include)

if (${ENABLE_CYACAS_BENCHMARKS})
    add_subdirectory (benchmark)
endif ()

if (${ENABLE_CYACAS_UNIT_TESTS})
    add_subdirectory (test)
endif ()