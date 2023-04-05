// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

#include <iostream>

#include <gtest/gtest.h>

#include <bio/alphabet/nucleotide/dna5.hpp>
#include <bio/ranges/concept.hpp>
#include <bio/ranges/to.hpp>
#include <bio/ranges/views/to_rank.hpp>

using namespace bio::alphabet::literals;

TEST(view_to_rank, basic)
{
    std::vector<bio::alphabet::dna5> vec{"ACTTTGATA"_dna5};
    std::vector<uint8_t>             cmp{0, 1, 4, 4, 4, 2, 0, 4, 0};

    // pipe notation
    std::vector<uint8_t> v = vec | bio::ranges::views::to_rank | bio::ranges::to<std::vector>();
    EXPECT_EQ(cmp, v);

    // function notation
    std::vector<uint8_t> v2(bio::ranges::views::to_rank(vec) | bio::ranges::to<std::vector>());
    EXPECT_EQ(cmp, v2);

    // combinability
    std::vector<uint8_t> cmp2{0, 4, 0, 2, 4, 4, 4, 1, 0};
    std::vector<uint8_t> v3 = vec | bio::ranges::views::to_rank | std::views::reverse | bio::ranges::to<std::vector>();
    EXPECT_EQ(cmp2, v3);
}

TEST(view_to_rank, concepts)
{
    std::vector<bio::alphabet::dna5> vec{"ACTTTGATA"_dna5};
    EXPECT_TRUE(std::ranges::input_range<decltype(vec)>);
    EXPECT_TRUE(std::ranges::forward_range<decltype(vec)>);
    EXPECT_TRUE(std::ranges::bidirectional_range<decltype(vec)>);
    EXPECT_TRUE(std::ranges::random_access_range<decltype(vec)>);
    EXPECT_FALSE(std::ranges::view<decltype(vec)>);
    EXPECT_TRUE(std::ranges::sized_range<decltype(vec)>);
    EXPECT_TRUE(std::ranges::common_range<decltype(vec)>);
    EXPECT_TRUE(bio::ranges::const_iterable_range<decltype(vec)>);
    EXPECT_TRUE((std::ranges::output_range<decltype(vec), bio::alphabet::dna5>));

    [[maybe_unused]] auto v1 = vec | bio::ranges::views::to_rank;
    EXPECT_TRUE(std::ranges::input_range<decltype(v1)>);
    EXPECT_TRUE(std::ranges::forward_range<decltype(v1)>);
    EXPECT_TRUE(std::ranges::bidirectional_range<decltype(v1)>);
    EXPECT_TRUE(std::ranges::random_access_range<decltype(v1)>);
    EXPECT_TRUE(std::ranges::view<decltype(v1)>);
    EXPECT_TRUE(std::ranges::sized_range<decltype(v1)>);
    EXPECT_TRUE(std::ranges::common_range<decltype(v1)>);
    EXPECT_TRUE(bio::ranges::const_iterable_range<decltype(v1)>);
    EXPECT_FALSE((std::ranges::output_range<decltype(v1), bio::alphabet::dna5>));
    EXPECT_FALSE((std::ranges::output_range<decltype(v1), uint8_t>));
}
