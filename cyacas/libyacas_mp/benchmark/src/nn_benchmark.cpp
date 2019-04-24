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

#include "yacas/mp/nn.hpp"

#include <benchmark/benchmark.h>

#include <algorithm>

std::mt19937_64 rng;

using namespace yacas::mp;

std::string random_string(std::size_t length)
{
    auto randchar = []() -> char
    {
        const char charset[] = "0123456789";
        const size_t max_index = (sizeof(charset) - 1);
        return charset[rand() % max_index];
    };
    std::string str(length, 0);
    std::generate_n(str.begin(), length, randchar);
    return str;
}

static void BM_NN_construct_random(benchmark::State& state)
{
    for (auto _: state) {
        NN b(state.range(), rng);
    }
    state.SetComplexityN(state.range());
}

static void BM_NN_parse(benchmark::State& state)
{
    for (auto _: state) {
        state.PauseTiming();
        const std::string s = random_string(state.range());
        state.ResumeTiming();
        NN b(s);
    }
    state.SetComplexityN(state.range());
}

static void BM_NN_to_string(benchmark::State& state)
{
    for (auto _: state) {
        state.PauseTiming();
        NN a(state.range(0), rng);
        state.ResumeTiming();
        a.to_string(10);
    }
    state.SetComplexityN(state.range());
}

static void BM_NN_shift_left(benchmark::State& state)
{
    for (auto _: state) {
        state.PauseTiming();
        NN a(state.range(0), rng);
        state.ResumeTiming();
        a <<= state.range(1);
    }
    state.SetComplexityN(state.range(0));
}

static void BM_NN_add(benchmark::State& state)
{
    for (auto _: state) {
        state.PauseTiming();
        NN a(state.range(0), rng);
        NN b(state.range(1), rng);
        state.ResumeTiming();
        a += b;
    }
    state.SetComplexityN(std::min(state.range(0), state.range(1)));
}

static void BM_NN_add_self(benchmark::State& state)
{
    for (auto _: state) {
        state.PauseTiming();
        NN a(state.range(0), rng);
        state.ResumeTiming();
        a += a;
    }
    state.SetComplexityN(state.range());
}

static void BM_NN_add_same(benchmark::State& state)
{
    for (auto _: state) {
        state.PauseTiming();
        NN a(random_string(state.range(0)));
        NN b(random_string(state.range(0)));
        state.ResumeTiming();
        a += b;
    }
    state.SetComplexityN(state.range());
}


static void BM_NN_sqr(benchmark::State& state)
{
    for (auto _: state) {
        state.PauseTiming();
        NN a(random_string(state.range(0)));
        state.ResumeTiming();
        a.sqr();
    }
    state.SetComplexityN(state.range());
}

static void BM_NN_mul_same(benchmark::State& state)
{
    for (auto _: state) {
        state.PauseTiming();
        NN a(random_string(state.range(0)));
        NN b(random_string(state.range(0)));
        state.ResumeTiming();
        a *= b;
    }
    state.SetComplexityN(state.range());
}

static void BM_NN_mul(benchmark::State& state)
{
    for (auto _: state) {
        state.PauseTiming();
        NN a(random_string(state.range(0)));
        NN b(random_string(state.range(1)));
        state.ResumeTiming();
        a *= b;
    }
    state.SetComplexityN(state.range(0) * state.range(1));
}

static void BM_NN_div(benchmark::State& state)
{
    for (auto _: state) {
        state.PauseTiming();
        NN a(random_string(state.range(0)));
        NN b(random_string(state.range(1)));
        if (b.is_zero())
            b = NN::ONE;
        state.ResumeTiming();
        a /= b;
    }
    state.SetComplexityN(state.range(0));
}


BENCHMARK(BM_NN_construct_random)->Range(1, 1<<16)->Complexity();
BENCHMARK(BM_NN_parse)->Range(1, 1<<14)->Complexity();
BENCHMARK(BM_NN_to_string)->Range(1, 1 << 16);
BENCHMARK(BM_NN_shift_left)->Ranges({{1, 1<<8}, {1, 1<<8}});
BENCHMARK(BM_NN_add)->Ranges({{1, 1<<8}, {1, 1<<8}});
BENCHMARK(BM_NN_add_self)->Range(1, 16);
BENCHMARK(BM_NN_add_self)->Range(16, 1<<10);
BENCHMARK(BM_NN_add_same)->Range(1, 16);
BENCHMARK(BM_NN_add_same)->Range(16, 1<<10);
BENCHMARK(BM_NN_sqr)->Range(1, 1<<16);
BENCHMARK(BM_NN_mul_same)->Range(1, 16);
BENCHMARK(BM_NN_mul_same)->Range(16, 1<<10);
BENCHMARK(BM_NN_mul)->Ranges({{1, 1<<8}, {1, 1<<8}});
BENCHMARK(BM_NN_div)->Ranges({{1, 1<<8}, {1, 1<<8}});

BENCHMARK_MAIN();
