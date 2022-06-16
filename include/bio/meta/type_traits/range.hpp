// -----------------------------------------------------------------------------------------------------
// Copyright (c) 2022 deCODE Genetics
// Copyright (c) 2006-2020, Knut Reinert & Freie Universität Berlin
// Copyright (c) 2016-2020, Knut Reinert & MPI für molekulare Genetik
// This file may be used, modified and/or redistributed under the terms of the 3-clause BSD-License
// shipped with this file and also available at: https://github.com/biocpp/biocpp-core/blob/main/LICENSE.md
// -----------------------------------------------------------------------------------------------------

/*!\file
 * \brief Provides various transformation traits used by the range module.
 * \author Hannes Hauswedell <hannes.hauswedell AT decode.is>
 */

#pragma once

#include <type_traits>

#include <bio/meta/platform.hpp>
#include <bio/meta/type_traits/basic.hpp>
#include <bio/meta/type_traits/iterator.hpp>
#include <ranges>
#include <iterator>

// TODO(h-2): add innermost_reference instead of or addition to range_innermost_value?

//NOTE(h-2): for the range overloads we explicitly forbid that the type is iteratoer
// because some types are actually both (e.g. std::directory_iterator)

namespace bio::detail
{

//!\cond
template <typename t>
concept has_range_value_type = requires { typename std::ranges::range_value_t<std::remove_cvref_t<t>>; };
//!\endcond

} // namespace bio::detail

namespace bio
{

/*!\addtogroup type_traits
 * \{
 */

// ----------------------------------------------------------------------------
// range_innermost_value
// ----------------------------------------------------------------------------

//NOTE(h-2): this could be moved to a separate file, because it also applies to iterators

/*!\brief Recursively determines the `value_type` on containers and/or iterators.
 * \implements bio::transformation_trait
 * \tparam t The type to recurse on; must have `std::ranges::value_type_t<rng_t>`.
 *
 * \details
 *
 * Attention, this transformation trait implicitly removes cv-qualifiers on all value_types except the one returned.
 */
template <typename t>
//!\cond
    requires detail::has_range_value_type<t>
//!\endcond
struct range_innermost_value
{
    //!\brief The return type (recursion not shown).
    using type = std::ranges::range_value_t<std::remove_cvref_t<t>>;
};

//!\cond
template <typename t>
    requires (detail::has_range_value_type<t> && detail::has_range_value_type<std::ranges::range_value_t<std::remove_cvref_t<t>>>)
struct range_innermost_value<t>
{
    using type = typename range_innermost_value<std::ranges::range_value_t<std::remove_cvref_t<t>>>::type;
};
//!\endcond

//!\brief Shortcut for bio::range_innermost_value (transformation_trait shortcut).
//!\see bio::range_innermost_value
template <typename t>
using range_innermost_value_t = typename range_innermost_value<t>::type;

// ----------------------------------------------------------------------------
// range_dimension_v
// ----------------------------------------------------------------------------

//NOTE(h-2): this could be moved to a separate file, because it also applies to iterators

/*!\brief Returns the number of times you can call `bio::value_type_t` recursively on t (type trait).
 * \tparam t The type to be queried; must resolve `bio::value_type_t` at least once.
 *
 * \details
 *
 * Attention, this type trait implicitly removes cv-qualifiers and reference from the types it recurses on and
 * returns.
 */
template <typename t>
//!\cond
    requires detail::has_range_value_type<t>
//!\endcond
constexpr size_t range_dimension_v = 1;

//!\cond
template <typename t>
    requires (detail::has_range_value_type<t> && detail::has_range_value_type<std::ranges::range_value_t<std::remove_cvref_t<t>>>)
constexpr size_t range_dimension_v<t> = range_dimension_v<std::ranges::range_value_t<std::remove_cvref_t<t>>> + 1;
//!\endcond

// ----------------------------------------------------------------------------
// range_compatible
// ----------------------------------------------------------------------------

//NOTE(h-2): this could be moved to a separate file, because it also applies to iterators

/*!\interface bio::range_compatible <>
 * \brief Two types are "compatible" if their bio::range_dimension_v and their bio::range_innermost_value_t are
 * the same.
 *
 * \details
 *
 * \include test/snippet/meta/type_traits/range.cpp
 *
 * Attention, this concept implicitly removes cv-qualifiers and reference from the types it recurses on and
 * compares.
 */
//!\cond
template <typename t1, typename t2>
concept range_compatible = requires (t1, t2)
{
    requires (range_dimension_v<t1> == range_dimension_v<t2>);

    requires std::is_same_v<range_innermost_value_t<t1>, range_innermost_value_t<t2>>;
};
//!\endcond

//!\}

} // namespace bio
