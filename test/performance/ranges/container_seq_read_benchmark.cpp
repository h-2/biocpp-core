// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md.md
// -----------------------------------------------------------------------------------------------------

#include <deque>
#include <list>
#include <vector>

#include <benchmark/benchmark.h>

#include <bio/alphabet/all.hpp>
#include <bio/ranges/container/all.hpp>
#include <bio/test/performance/sequence_generator.hpp>

template <typename t>
using sdsl_int_vec = sdsl::int_vector<sizeof(t) * 8>;

template <typename t>
using small_vec = bio::small_vector<t, 10'000>;

// ============================================================================
//  sequential_read
// ============================================================================

template <template <typename> typename container_t, typename alphabet_t, bool const_qualified = false>
void sequential_read(benchmark::State & state)
{
    auto cont_rando = bio::test::generate_sequence<alphabet_t>(10'000, 0, 0);
    container_t<alphabet_t> source(cont_rando.begin(), cont_rando.end());

    using source_ref_t = std::conditional_t<const_qualified,
                                            container_t<alphabet_t> const &,
                                            container_t<alphabet_t> &>;

    source_ref_t source_ref{source};

    alphabet_t a;
    for (auto _ : state)
        for (auto && c : source_ref)
            benchmark::DoNotOptimize(a = c);

    state.counters["sizeof"] = sizeof(alphabet_t);
    if constexpr (bio::alphabet<alphabet_t>)
        state.counters["alph_size"] = bio::alphabet_size<alphabet_t>;
    state.counters["const"] = const_qualified;
}

BENCHMARK_TEMPLATE(sequential_read, std::vector, char);
BENCHMARK_TEMPLATE(sequential_read, std::vector, uint8_t);
BENCHMARK_TEMPLATE(sequential_read, std::vector, uint16_t);
BENCHMARK_TEMPLATE(sequential_read, std::vector, uint32_t);
BENCHMARK_TEMPLATE(sequential_read, std::vector, uint64_t);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::gap);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::dna4);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::gapped<bio::dna4>);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::dna15);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::aa27);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::alphabet_variant<char, bio::dna4>);

BENCHMARK_TEMPLATE(sequential_read, std::deque, char);
BENCHMARK_TEMPLATE(sequential_read, std::deque, uint8_t);
BENCHMARK_TEMPLATE(sequential_read, std::deque, uint16_t);
BENCHMARK_TEMPLATE(sequential_read, std::deque, uint32_t);
BENCHMARK_TEMPLATE(sequential_read, std::deque, uint64_t);
BENCHMARK_TEMPLATE(sequential_read, std::deque, bio::gap);
BENCHMARK_TEMPLATE(sequential_read, std::deque, bio::dna4);
BENCHMARK_TEMPLATE(sequential_read, std::deque, bio::gapped<bio::dna4>);
BENCHMARK_TEMPLATE(sequential_read, std::deque, bio::dna15);
BENCHMARK_TEMPLATE(sequential_read, std::deque, bio::aa27);
BENCHMARK_TEMPLATE(sequential_read, std::deque, bio::alphabet_variant<char, bio::dna4>);

BENCHMARK_TEMPLATE(sequential_read, std::list, char);
BENCHMARK_TEMPLATE(sequential_read, std::list, uint8_t);
BENCHMARK_TEMPLATE(sequential_read, std::list, uint16_t);
BENCHMARK_TEMPLATE(sequential_read, std::list, uint32_t);
BENCHMARK_TEMPLATE(sequential_read, std::list, uint64_t);
BENCHMARK_TEMPLATE(sequential_read, std::list, bio::gap);
BENCHMARK_TEMPLATE(sequential_read, std::list, bio::dna4);
BENCHMARK_TEMPLATE(sequential_read, std::list, bio::gapped<bio::dna4>);
BENCHMARK_TEMPLATE(sequential_read, std::list, bio::dna15);
BENCHMARK_TEMPLATE(sequential_read, std::list, bio::aa27);
BENCHMARK_TEMPLATE(sequential_read, std::list, bio::alphabet_variant<char, bio::dna4>);

BENCHMARK_TEMPLATE(sequential_read, sdsl_int_vec, uint8_t);
BENCHMARK_TEMPLATE(sequential_read, sdsl_int_vec, uint16_t);
BENCHMARK_TEMPLATE(sequential_read, sdsl_int_vec, uint32_t);
BENCHMARK_TEMPLATE(sequential_read, sdsl_int_vec, uint64_t);

BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, char);
BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::gap);
BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::dna4);
//TODO(bio): works on gcc10, fails on gc11
// BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::gapped<bio::dna4>);
BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::dna15);
BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::aa27);
//TODO(bio): works on gcc10, fails on gc11
// BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::alphabet_variant<char, bio::dna4>);

BENCHMARK_TEMPLATE(sequential_read, small_vec, char);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::gap);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::dna4);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::gapped<bio::dna4>);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::dna15);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::aa27);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::alphabet_variant<char, bio::dna4>);

// ============================================================================
//  sequential_read (const)
// ============================================================================

BENCHMARK_TEMPLATE(sequential_read, std::vector, char, true);
BENCHMARK_TEMPLATE(sequential_read, std::vector, uint8_t, true);
BENCHMARK_TEMPLATE(sequential_read, std::vector, uint16_t, true);
BENCHMARK_TEMPLATE(sequential_read, std::vector, uint32_t, true);
BENCHMARK_TEMPLATE(sequential_read, std::vector, uint64_t, true);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::gap, true);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::dna4, true);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::gapped<bio::dna4>, true);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::dna15, true);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::aa27, true);
BENCHMARK_TEMPLATE(sequential_read, std::vector, bio::alphabet_variant<char, bio::dna4>, true);

BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, char, true);
BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::gap, true);
BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::dna4, true);
//TODO(bio): works on gcc10, fails on gc11
// BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::gapped<bio::dna4>, true);
BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::dna15, true);
BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::aa27, true);
//TODO(bio): works on gcc10, fails on gc11
// BENCHMARK_TEMPLATE(sequential_read, bio::bitcompressed_vector, bio::alphabet_variant<char, bio::dna4>, true);

BENCHMARK_TEMPLATE(sequential_read, small_vec, char, true);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::gap, true);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::dna4, true);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::gapped<bio::dna4>, true);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::dna15, true);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::aa27, true);
BENCHMARK_TEMPLATE(sequential_read, small_vec, bio::alphabet_variant<char, bio::dna4>, true);

// ============================================================================
//  run
// ============================================================================

BENCHMARK_MAIN();
