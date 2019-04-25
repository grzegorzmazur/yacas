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

find_package (Threads REQUIRED)
find_package (benchmark REQUIRED)

add_executable (yacas_mp_nn_benchmark src/nn_benchmark.cpp)
target_link_libraries (yacas_mp_nn_benchmark libyacas_mp benchmark::benchmark benchmark::benchmark_main Threads::Threads)

add_custom_target(bench COMMAND yacas_mp_nn_benchmark --benchmark_min_time=0.1)
