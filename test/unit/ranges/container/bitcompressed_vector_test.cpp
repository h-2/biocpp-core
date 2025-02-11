// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

#include <gtest/gtest.h>

#include <bio/alphabet/nucleotide/concept.hpp>
#include <bio/alphabet/nucleotide/dna4.hpp>
#include <bio/ranges/views/complement.hpp>
#include <bio/test/expect_range_eq.hpp>

#include "container_test_template.hpp"

INSTANTIATE_TYPED_TEST_SUITE_P(bitcompressed, container_over_dna4_test, bio::bitcompressed_vector<bio::dna4>, );

using bio::operator""_dna4;

TEST(bitcompressed_vector_test, issue1743_complement_on_proxy)
{ // https://github.com/seqan/seqan3/issues/1743
    bio::bitcompressed_vector<bio::dna4> v{'A'_dna4};

    auto proxy = *v.begin();
    auto complement = bio::complement(proxy);

    EXPECT_TRUE((std::same_as<decltype(complement), bio::dna4>));
    EXPECT_EQ(complement, 'T'_dna4);
}

TEST(bitcompressed_vector_test, issue1743_view_combinability)
{ // https://github.com/seqan/seqan3/issues/1743
    bio::bitcompressed_vector<bio::dna4> v{'A'_dna4, 'C'_dna4, 'G'_dna4, 'T'_dna4};
    auto complement = v | bio::views::complement;

    EXPECT_EQ(v.size(), complement.size());
    EXPECT_RANGE_EQ(complement, (bio::dna4_vector{'T'_dna4, 'G'_dna4, 'C'_dna4, 'A'_dna4}));
}
