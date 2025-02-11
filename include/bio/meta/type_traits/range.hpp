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
#include <bio/meta/type_traits/pre.hpp>
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
// value_type
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
/*!\brief Exposes the `value_type` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query; must model std::ranges::input_range.
 * \deprecated This is deprecated use std::ranges::range_value_t.
 */
template <std::ranges::input_range rng_t>
//!\cond
    requires (!std::input_or_output_iterator<rng_t>)
//!\endcond
struct value_type<rng_t>
{
    //!\brief Return the value_type member definition from the queried type's iterator.
    using type = value_type_t<std::ranges::iterator_t<rng_t>>;
};
} // namespace bio::detail
#endif // BIOCPP_DEPRECATED_310

// ----------------------------------------------------------------------------
// reference
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
/*!\brief Exposes the `reference` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query; must model std::ranges::input_range.
 * \deprecated This is deprecated use std::ranges::range_reference_t.
 */
template <std::ranges::input_range rng_t>
//!\cond
    requires (!std::input_or_output_iterator<rng_t>)
//!\endcond
struct reference<rng_t>
{
    //!\brief Return the reference member definition from the queried type's iterator.
    using type = reference_t<std::ranges::iterator_t<rng_t>>;
};
 } // namespace bio::detail
 #endif // BIOCPP_DEPRECATED_310

// ----------------------------------------------------------------------------
// rvalue_reference
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
/*!\brief Exposes the `rvalue_reference` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query; must model std::ranges::input_range.
 * \deprecated This is deprecated use std::ranges::range_rvalue_reference_t.
 */
template <std::ranges::input_range rng_t>
//!\cond
    requires (!std::input_or_output_iterator<rng_t>)
//!\endcond
struct rvalue_reference<rng_t>
{
    //!\brief Return the rvalue_reference member definition from the queried type's iterator.
    using type = rvalue_reference_t<std::ranges::iterator_t<rng_t>>;
};
} // namespace bio::detail
#endif // BIOCPP_DEPRECATED_310

// ----------------------------------------------------------------------------
// const_reference
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
/*!\brief Exposes the `const_reference` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query; must model std::ranges::input_range.
 * \deprecated This is deprecated use std::ranges::range_reference_t<rng_t const>.
 */
template <std::ranges::input_range rng_t>
//!\cond
    requires (!std::input_or_output_iterator<rng_t>)
//!\endcond
struct const_reference<rng_t>
{
    //!\brief Resolves to the reference type of the `const_iterator` of t (not the `const iterator`!).
    using type = std::iter_reference_t<std::ranges::iterator_t<rng_t const>>;
};
} // namespace bio::detail
#endif // BIOCPP_DEPRECATED_310

// ----------------------------------------------------------------------------
// difference_type
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
/*!\brief Exposes the `difference_type` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query; must model std::ranges::input_range.
 * \deprecated This is deprecated use std::ranges::range_difference_t.
 */
template <std::ranges::range rng_t>
//!\cond
    requires (!std::input_or_output_iterator<rng_t>)
//!\endcond
struct difference_type<rng_t>
{
    //!\brief Return the difference_type member definition from the queried type's iterator.
    using type = difference_type_t<std::ranges::iterator_t<rng_t>>;
};
} // namespace bio::detail
#endif // BIOCPP_DEPRECATED_310

// ----------------------------------------------------------------------------
// size_type
// ----------------------------------------------------------------------------

#ifdef BIOCPP_DEPRECATED_310
namespace detail
{
/*!\brief Exposes the `size_type` of another type.
 * \implements bio::transformation_trait
 * \tparam t The type you wish to query; must model std::ranges::sized_range.
 * \deprecated This is deprecated use std::ranges::range_size_t.
 */
template <std::ranges::sized_range rng_t>
//!\cond
    requires (!std::input_or_output_iterator<rng_t>)
//!\endcond
struct size_type<rng_t>
{
    //!\brief Return the size_type as returned by the size function.
    using type = decltype(std::ranges::size(std::declval<rng_t &>()));
};
} // namespace bio::detail
#endif // BIOCPP_DEPRECATED_310

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
    requires detail::has_range_value_type<t> && detail::has_range_value_type<std::ranges::range_value_t<std::remove_cvref_t<t>>>
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
    requires detail::has_range_value_type<t> && detail::has_range_value_type<std::ranges::range_value_t<std::remove_cvref_t<t>>>
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
