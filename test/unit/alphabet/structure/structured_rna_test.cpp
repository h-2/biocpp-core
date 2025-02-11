// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

#include <gtest/gtest.h>

#include <bio/alphabet/concept.hpp>
#include <bio/alphabet/nucleotide/rna4.hpp>
#include <bio/alphabet/nucleotide/rna5.hpp>
#include <bio/alphabet/structure/dot_bracket3.hpp>
#include <bio/alphabet/structure/structured_rna.hpp>
#include <bio/alphabet/structure/wuss.hpp>

#include "../alphabet_constexpr_test_template.hpp"
#include "../alphabet_test_template.hpp"
#include "../semi_alphabet_constexpr_test_template.hpp"
#include "../semi_alphabet_test_template.hpp"
#include "../composite/alphabet_tuple_base_test_template.hpp"

template <typename rna_type, typename structure_type>
class alphabet_tuple_base_test<bio::structured_rna<rna_type, structure_type>> : public ::testing::Test
{
public:
    using T = bio::structured_rna<rna_type, structure_type>;

    using dna_type = std::conditional_t<std::is_same_v<rna_type, bio::rna4>, bio::dna4, bio::dna5>;

    T instance = T{value_1(), value_2()};
    T zero_instance = T{decltype(value_1()){}, decltype(value_2()){}};
    size_t tup_size{2};

    // bio::structured_rna<rna_type, structure_type>
    // -------------------------------------------------------------------------
    rna_type value_1()
    {
        return rna_type{}.assign_char('G');
    }
    dna_type assignable_to_value_1()
    {
        return dna_type{}.assign_char('G');
    }
    structure_type value_2()
    {
        return structure_type{}.assign_char('(');
    }
    structure_type assignable_to_value_2()
    {
        return structure_type{}.assign_char('('); // replace if assignable subtype becomes available
    }
    auto values_to_cmp()
    {
        return std::make_tuple(/*low */rna_type{}.assign_char('A'), structure_type{}.assign_char('.'),
                               /*mid */rna_type{}.assign_char('C'), structure_type{}.assign_char('('),
                               /*high*/rna_type{}.assign_char('T'), structure_type{}.assign_char(')'));
    }
};

using structured_rna_types = ::testing::Types<bio::structured_rna<bio::rna5, bio::dot_bracket3>,
                                              bio::structured_rna<bio::rna4, bio::wuss51>>;

INSTANTIATE_TYPED_TEST_SUITE_P(structured_rna, alphabet, structured_rna_types, );
INSTANTIATE_TYPED_TEST_SUITE_P(structured_rna, semi_alphabet_test, structured_rna_types, );
INSTANTIATE_TYPED_TEST_SUITE_P(structured_rna, alphabet_constexpr, structured_rna_types, );
INSTANTIATE_TYPED_TEST_SUITE_P(structured_rna, semi_alphabet_constexpr, structured_rna_types, );
INSTANTIATE_TYPED_TEST_SUITE_P(structured_rna, alphabet_tuple_base_test, structured_rna_types, );
