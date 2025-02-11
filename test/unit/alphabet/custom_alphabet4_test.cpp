// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

#include <gtest/gtest.h>

#include <bio/alphabet/concept.hpp>

struct non_writeable_alphabet
{
    using rank_t = uint8_t;
    using char_t = char;

    rank_t to_rank() const noexcept;
    char_t to_char() const noexcept;

    constexpr static bool alphabet_size{1};

    friend bool operator<(non_writeable_alphabet, non_writeable_alphabet);
    friend bool operator<=(non_writeable_alphabet, non_writeable_alphabet);
    friend bool operator>(non_writeable_alphabet, non_writeable_alphabet);
    friend bool operator>=(non_writeable_alphabet, non_writeable_alphabet);
    friend bool operator==(non_writeable_alphabet, non_writeable_alphabet);
    friend bool operator!=(non_writeable_alphabet, non_writeable_alphabet);
};


// see issue https://github.com/seqan/seqan3/issues/1518
TEST(non_writeable_alphabet_test, issue1518)
{
    EXPECT_TRUE(bio::semialphabet<non_writeable_alphabet>);
    EXPECT_TRUE(bio::alphabet<non_writeable_alphabet>);

    EXPECT_FALSE(bio::writable_semialphabet<non_writeable_alphabet>);
    EXPECT_FALSE(bio::writable_alphabet<non_writeable_alphabet>);
}
