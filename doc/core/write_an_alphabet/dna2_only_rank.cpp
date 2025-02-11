// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

//! [struct]
#include <bio/alphabet/concept.hpp>                   // alphabet concept checks

struct dna2
{
    uint8_t rank{};
};
//! [struct]

//! [alphabet_concept]
static_assert(bio::alphabet<dna2> == false);          // NOT an alphabet
//! [alphabet_concept]

//! [other_concepts]
static_assert(std::copy_constructible<dna2>);             // ok
static_assert(std::totally_ordered<dna2> == false); // NO comparison operators
static_assert(bio::semialphabet<dna2> == false);      // NOT a semialphabet
static_assert(bio::alphabet<dna2> == false);          // NOT an alphabet
//! [other_concepts]
