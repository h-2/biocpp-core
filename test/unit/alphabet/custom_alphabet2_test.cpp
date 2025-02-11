// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

#include <bio/meta/platform.hpp>

// Tests the ADL-capabilities of the alphabet customisation point objects; type that isn't default constructible

//![my_alph]
#include <cstddef>                      // for size_t
#include <bio/alphabet/concept.hpp>  // for bio::alphabet
#include <type_traits>       // for std::type_identity

namespace my_namespace
{

class my_alph
{
public:
    bool rank;

    my_alph() = delete;
    constexpr my_alph(my_alph const &) = default;
    constexpr my_alph & operator=(my_alph const &) = default;

    constexpr my_alph(bool rank) : rank{rank} {}

    constexpr friend bool operator==(my_alph lhs, my_alph rhs) { return lhs.rank == rhs.rank; }
    constexpr friend bool operator!=(my_alph lhs, my_alph rhs) { return lhs.rank != rhs.rank; }
    constexpr friend bool operator<=(my_alph lhs, my_alph rhs) { return lhs.rank <= rhs.rank; }
    constexpr friend bool operator>=(my_alph lhs, my_alph rhs) { return lhs.rank >= rhs.rank; }
    constexpr friend bool operator< (my_alph lhs, my_alph rhs) { return lhs.rank <  rhs.rank; }
    constexpr friend bool operator> (my_alph lhs, my_alph rhs) { return lhs.rank >  rhs.rank; }
};


constexpr size_t alphabet_size(std::type_identity<my_alph> const &) noexcept
{
    return 2;
}

constexpr bool to_rank(my_alph const a) noexcept
{
    return a.rank;
}

constexpr my_alph & assign_rank_to(bool const r, my_alph & a) noexcept
{
    a.rank = r;
    return a;
}

constexpr char to_char(my_alph const a) noexcept
{
    if (a.rank)
        return '1';
    else
        return '0';
}

constexpr my_alph & assign_char_to(char const c, my_alph & a) noexcept
{
    switch (c)
    {
        case '0': case 'F': case 'f': a.rank = 0; return a;
        default: a.rank = 1; return a;
    }
}

constexpr bool char_is_valid_for(char const c, std::type_identity<my_alph> const &) noexcept
{
    switch (c)
    {
        case '0': case 'F': case 'f': case '1': case 'T': case 't': return true;
        default: return false;
    }
}

} // namespace my_namespace

static_assert(bio::alphabet_size<my_namespace::my_alph> == 2);
static_assert(bio::char_is_valid_for<my_namespace::my_alph>('T'));
static_assert(!bio::char_is_valid_for<my_namespace::my_alph>('!'));
static_assert(bio::semialphabet<my_namespace::my_alph>);
static_assert(bio::alphabet<my_namespace::my_alph>);
//![my_alph]

// Not tested with rest of test-suite because the test-suite relies on default-constructibility
